// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace FSharp.Control

#if NET40

open System.Threading.Tasks

module Task =   
    let start (t:Task<_>) = t.Start() 
    let await (t:Task<_>) = 
        Async.AwaitTask t
        
    let map f (t:Task<_>) =         
        if t.Status = TaskStatus.Created then
            new Task<_>(fun () ->
                let sched = TaskScheduler.Current
                t.Start(sched)
                f t.Result)
        else
            t.ContinueWith(fun (t:Task<_>) -> t.Result |> f)
            
    let continueWith (f:Task<_> -> _) (t:Task<_>) = 
        t.ContinueWith(f)
        
    let ofAsync asy = new Task<_>(fun () -> 
        let t = asy |> Async.StartAsTask
        t.Result)
        
    /// Starts the given tasks and returns them in the order they finish.
    /// The tasks will be started as soon as they are available and they will be returned as they finish.
    /// The task will start when you start to iterate over the results.
    let startTasks (waitForTasks:AsyncSeq<Task<_>>) = 
      asyncSeq {
        let source = new ObservableSource<_>()
        // Wait for the tasks to become available
        // Start tasks
        async {
            // Wait for all tasks to finish to send complete
            
            // Wait a time to let them attach to source
            do! Async.Sleep 10
            let cache =
                waitForTasks
                    // NOTE: maybe we should do it with "Task.map" but I don't feel like the above map implementation is good enough.
                    |> AsyncSeq.map (fun task -> 
                        if task.Status = TaskStatus.Created then
                            start task
                        task)
                    |> AsyncSeq.map  
                        (continueWith
                            (fun t -> 
                                if t.IsFaulted then source.Error t.Exception
                                if t.IsCompleted && not t.IsFaulted then source.Next t.Result))
                    |> AsyncSeq.cache
            // Start all tasks
            do! cache 
                    |> AsyncSeq.iter (fun _ -> ())
            
            // Wait for all to finish
            do! cache
                    |> AsyncSeq.iterAsync (await >> Async.Ignore)
            source.Completed()
        } |> Async.Start
        // Note here we start to listen on the source
        yield! source.AsObservable |> AsyncSeq.ofObservableBuffered }
#endif