// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace FSharpx.Logging

open System
open System.Diagnostics
open System.IO
open System.Xml
open System.Xml.XPath
open System.Threading

module XmlWriterHelper =
    type Context = {
        Writer : XmlWriter
        Namespace : string }
    type Writer = Context -> unit
    let combine f1 f2 = (fun context ->
        f1 context
        f2 context)
    let Empty = (fun context -> ())
    let Elem name f = (fun context ->
        let w = context.Writer
        w.WriteStartElement(name, context.Namespace)
        f context
        w.WriteEndElement()
        )
    let String value = (fun context ->
        let w = context.Writer
        w.WriteString(value)
        )
        
    let rec And (l:Writer list) = 
        match l with
        | [] -> (fun context -> ())
        | x::xs -> combine (And xs) x
        
    let Attribute name value = (fun context ->
        let w = context.Writer
        w.WriteAttributeString (name, value);
        )
    let Namespace ns f = (fun context ->
        f { context with Namespace = ns })
       
    let Raw text = 
        (fun context -> context.Writer.WriteRaw text) 
        
open XmlWriterHelper

type SimpleXmlWriterTraceListener(initData:string, name:string) as x = 
    inherit System.Diagnostics.TextWriterTraceListener(name)
    static let defaultName = "SimpleXmlWriter"
    static let xmlwritersettings = new XmlWriterSettings ( OmitXmlDeclaration = true )
    
    let stream = new StreamWriter (new FileStream (initData, FileMode.Append, FileAccess.Write, FileShare.ReadWrite))
    let w = XmlWriter.Create (stream, xmlwritersettings)
    
    let nsE2E = "http://schemas.microsoft.com/2004/06/E2ETraceEvent"
    let nsSystem = "http://schemas.microsoft.com/2004/06/windows/eventlog/system"
    let xpathNavToString (nav:XPathNavigator) = 
        let sw = new StringWriter()
        use xw = XmlWriter.Create(sw, xmlwritersettings)
        nav.WriteSubtree(xw)
        sw.ToString()
        
    let traceCore (eventCache:TraceEventCache) (source:string) (eventType:TraceEventType)
        (id:int) (relatedActivity:Guid option) (level:int) (wrapData:bool) (data:obj array) =
        let p = 
            if eventCache <> null 
            then Process.GetProcessById(eventCache.ProcessId)
            else Process.GetCurrentProcess()
        
        let date = 
            XmlConvert.ToString ((if eventCache <> null then eventCache.DateTime else DateTime.Now), XmlDateTimeSerializationMode.Unspecified)
        
        let xmlWriter = 
            Elem "E2ETraceEvent" 
                (And [
                    Namespace nsSystem (Elem "System" 
                        (And [
                            Elem "EventId" (String (XmlConvert.ToString(id)))
                            Elem "Type" (String "3")
                            Elem "SubType" 
                                (And [
                                    Attribute "Name" (eventType.ToString())
                                    String "0"
                                ])
                            Elem "Level" (String (level.ToString()))
                            Elem "TimeCreated" (String date)
                            Elem "Source" (String source)
                            Elem "Correlation" 
                                (And [
                                    yield Attribute "ActivityID" (sprintf "{%O}" Trace.CorrelationManager.ActivityId)
                                    match relatedActivity with
                                    | Some id -> 
                                        yield Attribute "RelatedActivityID" (sprintf "{%O}" Trace.CorrelationManager.ActivityId)
                                    | None -> ()
                                ])
                            Elem "Execution" 
                                (And [
                                    Attribute "ProcessName" p.MainModule.ModuleName
                                    Attribute "ProcessId" (p.Id.ToString())
                                    Attribute "ThreadID" (if eventCache <> null && not <| System.String.IsNullOrWhiteSpace eventCache.ThreadId then eventCache.ThreadId else Thread.CurrentThread.ManagedThreadId.ToString ())                        
                                ])
                            Elem "Channel" Empty
                            Elem "Computer" (String p.MachineName)
                        ]))
                    Elem "ApplicationData" 
                        (Elem "TraceData"
                            (And [
                                 for o in data do
                                    let rawWriter =
                                        match o with
                                        | :? XPathNavigator as nav -> 
                                            // the output ignores xmlns difference between the parent (E2ETraceEvent and the content node).
                                            // To clone such behavior, I took this approach.
                                            Raw (xpathNavToString nav)
                                        | _ -> String (o.ToString())
                                    yield
                                        if wrapData
                                        then Elem "DataItem" rawWriter
                                        else rawWriter
                            ]))
                ])
        
        // write it
        xmlWriter { Namespace = nsE2E; Writer = w }   
        w.Flush()
        x.Flush()
        ()
    
    new(file:string) = new SimpleXmlWriterTraceListener(file, defaultName)
    //new(file:string, name) = new SimpleXmlWriterTraceListener(file, name)
    //new(stream:Stream) = new SimpleXmlWriterTraceListener(stream, defaultName)
    //new(stream:Stream, name) = new SimpleXmlWriterTraceListener(stream, name)
    //new(writer:TextWriter) = new SimpleXmlWriterTraceListener(writer, defaultName)
    interface IDuplicateListener with
        member x.Duplicate name =
            let newPath = CopyListenerHelper.createNewFilename initData name
            CopyListenerHelper.copyListener 
                x
                (new SimpleXmlWriterTraceListener(newPath) :> TraceListener)
    
    override x.Close () = w.Close()
    override x.Fail (msg, detail) =
         x.TraceEvent(null, null, TraceEventType.Error, 0, System.String.Concat(msg, " ", detail))
     
    override x.TraceData(cache, source, eventType, id, data:obj) = 
        traceCore cache source eventType id None 2 true [|data|]
    override x.TraceData(cache, source, eventType, id, [<ParamArray>] data:obj array) =
        traceCore cache source eventType id None 2 true data

    override x.TraceEvent(cache, source, eventType, id, message:string) =
        traceCore cache source eventType id None 2 true [|message|]
    override x.TraceEvent(cache, source, eventType, id, format:string, [<ParamArray>] args:obj array) =
        traceCore cache source eventType id None 2 true [| System.String.Format(format, args) |]
    
    
    override x.TraceTransfer(cache, source, id, message, relatedId) =
        traceCore cache source TraceEventType.Transfer id (Some relatedId) 255 true [|message|]
     
    override x.Write(message) =
        x.WriteLine(message)
    
    override x.WriteLine(message:string) =
        traceCore null "Trace" TraceEventType.Information 0 None 8 false [|message|]

    
    
    
    
    
    