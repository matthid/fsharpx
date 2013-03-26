// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace FSharpx.Logging

open System
open System.Diagnostics
open System.IO

type ITracer = 
    inherit IDisposable
    abstract member TraceSource : TraceSource
    abstract member ActivityId : Guid
    
type IDuplicateListener = 
    abstract member Duplicate : string -> TraceListener

module CopyListenerHelper = 
    let copyListener (fromListener:TraceListener) (toListener:TraceListener) = 
        toListener.Attributes.Clear()
        for pair in 
            fromListener.Attributes.Keys
                |> Seq.cast
                |> Seq.map2 (fun k v -> k,v) (fromListener.Attributes.Values |> Seq.cast) do
            toListener.Attributes.Add pair
        toListener.Filter <- fromListener.Filter
        toListener.IndentLevel <- fromListener.IndentLevel
        toListener.Name <- fromListener.Name
        toListener.TraceOutputOptions <- fromListener.TraceOutputOptions
        toListener

    let createNewFilename oldRelFilePath name =
        if System.String.IsNullOrEmpty(oldRelFilePath) then null
        else
            let fileName = Path.GetFileNameWithoutExtension(oldRelFilePath)
            let extension = Path.GetExtension(oldRelFilePath)
            Path.Combine(
                Path.GetDirectoryName(oldRelFilePath),
                sprintf "%s.%s%s" fileName name extension)

type DefaultTraceListener(initData:string, name:string) = 
    inherit System.Diagnostics.DefaultTraceListener()
    new() = new DefaultTraceListener(null,null)
    new(s) = new DefaultTraceListener(s,s)
    interface IDuplicateListener with
        member x.Duplicate name =
            CopyListenerHelper.copyListener
                x
                (new DefaultTraceListener() :> TraceListener)

type XmlWriterTraceListener(initData:string, name:string) = 
    inherit System.Diagnostics.XmlWriterTraceListener(initData)
    new(s) = new XmlWriterTraceListener(s,s)
    interface IDuplicateListener with
        member x.Duplicate name =
            let newPath = CopyListenerHelper.createNewFilename initData name
            CopyListenerHelper.copyListener 
                x
                (new XmlWriterTraceListener(newPath) :> TraceListener)
type TextWriterTraceListener(initData:string, name:string) = 
    inherit System.Diagnostics.TextWriterTraceListener(initData)
    new(s) = new TextWriterTraceListener(s,s)
    interface IDuplicateListener with
        member x.Duplicate name =
            let newPath = CopyListenerHelper.createNewFilename initData name
            CopyListenerHelper.copyListener 
                x
                (new TextWriterTraceListener(newPath) :> TraceListener)

type ConsoleTraceListener(initData:string, name:string) = 
    inherit System.Diagnostics.ConsoleTraceListener()
    new(s) = new ConsoleTraceListener(s,s)
    interface IDuplicateListener with
        member x.Duplicate name =
            let newPath = CopyListenerHelper.createNewFilename initData name
            CopyListenerHelper.copyListener 
                x
                (new ConsoleTraceListener(newPath) :> TraceListener)
(* We have to override TraceListener and delegate everything
type EventLogTraceListener(initData:string, name:string) = 
    inherit System.Diagnostics.TraceListener(initData)
    new(s) = new EventLogTraceListener(s,s)
    interface IDuplicateListener with
        member x.Duplicate name =
            let newPath = CopyListenerHelper.createNewFilename initData name
            CopyListenerHelper.copyListener 
                x
                (new EventLogTraceListener(newPath) :> TraceListener)*)


                                                                                                 
[<AutoOpen>]
module LogInterfaceExtensions =
    let L = sprintf
    /// executes the given action (a log function) on the given ActivityId
    let doOnActivity activity f =        
        let oldId = Trace.CorrelationManager.ActivityId
        try
            Trace.CorrelationManager.ActivityId <- activity
            f()
        finally
            Trace.CorrelationManager.ActivityId <- oldId

    type ITracer with 
        member x.doInId f = 
            doOnActivity x.ActivityId f
        member private x.logHelper ty (o : obj) =  
            x.doInId 
                (fun () ->
                    x.TraceSource.TraceEvent(ty, 0, "{0}", [|o|])
                    x.TraceSource.Flush())
        /// Logs a message with the given TraceEventType
        member x.log ty (fmt:unit -> string) =
            // call the formatting function only when we actually do logging 
            // (in which case ToString() is called)
            let helper = 
                { new obj() with
                    override x.ToString() = 
                        fmt () }
            x.logHelper ty helper
        /// Logs a TraceEventType.Verbose message
        member x.logVerb fmt = x.log System.Diagnostics.TraceEventType.Verbose fmt
        /// Logs a TraceEventType.Warning message
        member x.logWarn fmt = x.log System.Diagnostics.TraceEventType.Warning fmt
        /// Logs a TraceEventType.Critical message
        member x.logCrit fmt = x.log System.Diagnostics.TraceEventType.Critical fmt
        /// Logs a TraceEventType.Error message
        member x.logErr fmt =  x.log System.Diagnostics.TraceEventType.Error fmt
        /// Logs a TraceEventType.Information message
        member x.logInfo fmt = x.log System.Diagnostics.TraceEventType.Information fmt
    
   
    /// To be able to use the given configuration but write in different files.
    /// We want to write in different files per default because this class is designed for parallel environments.
    /// For example when in the configuration "file.log" is given we log to "file.name.log".    
    type MyTraceSource(traceEntry:string,name:string) as x= 
        inherit TraceSource(traceEntry)
        do 
            // NOTE: Shared Listeners are not supported
            // (currently we create new ones and do not share them the same way)
            let flags = System.Reflection.BindingFlags.Public |||
                        System.Reflection.BindingFlags.NonPublic ||| 
                        System.Reflection.BindingFlags.Instance
            let invalid = Path.GetInvalidFileNameChars() 
                                |> Seq.append (Path.GetInvalidPathChars())   
            let cleanPath = 
                (
                name 
                    |> Seq.fold 
                        (fun (builder:Text.StringBuilder) char -> 
                            builder.Append(
                                if invalid |> Seq.exists (fun i -> i = char) 
                                then '_'
                                else char))
                        (new System.Text.StringBuilder(name.Length))
                ).ToString()

            let eventCache = new TraceEventCache(); 

            let newTracers = [|
                for l in x.Listeners do
                    match l:>obj with
                    | :? IDuplicateListener as myListener ->
                        yield myListener.Duplicate(cleanPath)
                    | _ ->
                        l.TraceEvent(eventCache, x.Name, TraceEventType.Error, 0, sprintf "Unknown Listener, can't apply name \"%s\"" name)
                        yield l
                |]
            x.Listeners.Clear()
            x.Listeners.AddRange(newTracers)
            
    /// A simple ITracer implementation for a given tracesource
    let createDefaultStateTracer (traceSource:TraceSource) activityName = 
        let activityId = Guid.NewGuid()
        doOnActivity activityId (fun () -> traceSource.TraceEvent(TraceEventType.Start, 0, activityName))
        { new ITracer with  
            member x.TraceSource = traceSource
            member x.ActivityId = activityId
        
           interface IDisposable with
            member x.Dispose() = 
                doOnActivity activityId (fun () -> traceSource.TraceEvent(TraceEventType.Stop, 0, activityName)) }
     
    type ITracer with
        /// create a child tracer from the current instance
        /// we add a cross reference to the logfiles and return a new instance where we can start logging the activity
        member x.childTracer traceSource newActivity = 
            let tracer = createDefaultStateTracer traceSource newActivity
            x.doInId 
                (fun () -> 
                    x.TraceSource.TraceTransfer(0, "Switching to " + newActivity, tracer.ActivityId))
            tracer   
            
/// Provides a simple layer above the .net logging facilities
module Log =     
    /// Provides a more advanced Tracesource which allows to share a configuration (given by traceEntry) for multiple tracesources (distinguished by the name)
    let MySource traceEntry name = new MyTraceSource(traceEntry, name)
    /// Provides classical TraceSource logging facilities
    let Source entryName = new TraceSource(entryName)
    
    /// Wraps a TraceSource and provides a more F# friendly interface
    let DefaultTracer traceSource id = 
        createDefaultStateTracer traceSource id
        
    let EmptyTracer = 
        let emptySource = Source "Empty"
        emptySource.Listeners.Clear()
        DefaultTracer emptySource "Empty"
            

            
            
            
            