namespace FSharp.Control
open FSharp.Control
open System.Threading
type FinishAwareData<'a> = 
    | DataFinished
    | Data of 'a

/// represents an Agent that is able to shut down after everything is processed
type FinishAwareAgent<'a> private (f, ct) =
    let finishedEvent = new Event<_>()
    let finishedHandle = new ManualResetEvent(false)
    let mbox = 
        Agent<FinishAwareData<'a>>.Start(f (fun () -> finishedEvent.Trigger(); finishedHandle.Set() |> ignore), ct)
    let buildMessageHelper buildMessage = 
        (fun replyChannel -> Data <| buildMessage replyChannel)
    static let startParallelHelper ct workerNum (createWorker:unit -> FinishAwareAgent<_>) =      
        FinishAwareAgent<_>.Start(
            (fun finish (inbox:MailboxProcessor<_>) ->
                let workers = Array.init workerNum (fun i -> createWorker()) : FinishAwareAgent<_> array
                let rec loop currentNum = async {
                    let! msg = inbox.Receive()               
                    match msg with
                    | DataFinished ->
                        for i in 0..workerNum - 1 do
                            workers.[i].PostFinish()
                        do!
                            workers
                            |> Seq.map (fun w -> w.AwaitFinish())    
                            |> Async.Parallel
                            |> Async.Ignore
                        finish()
                        return ()
                    | Data d ->
                        workers.[currentNum].Post d
                        return! loop ((currentNum + 1) % workerNum)
                }
                loop 0), ct)
    static member Start(f, ct) = new FinishAwareAgent<_>(f, ct)
        
    static member StartSimple<'b>(initialState:'b, processData:'a -> 'b -> Async<'b>, ct) =
        FinishAwareAgent<_>.Start(
            (fun setFinished inbox ->
                let rec loop state = 
                    async {
                        let! data = inbox.Receive()
                        match data with
                        | Data d -> 
                            let! newState = processData d state
                            return! loop newState
                        | DataFinished ->
                            setFinished()
                            return ()
                    }
                loop initialState), ct)
    static member StartParallel(workerNum, f, ct) = startParallelHelper ct workerNum (fun () -> FinishAwareAgent<_>.Start(f, ct))
    static member StartParallelSimple(workerNum, initialState, processData, ct) = startParallelHelper ct workerNum (fun () -> FinishAwareAgent<_>.StartSimple(initialState, processData, ct))
    /// Returns the number of unprocessed messages in the message queue of the agent.
    member x.CurrentQueueLength = mbox.CurrentQueueLength
    
    member x.AwaitFinish() = finishedHandle |> Async.AwaitWaitHandle |> Async.Ignore
        
    /// Occurs when the mailbox has been finished.
    [<CLIEvent>]
    member x.Finished = finishedEvent.Publish

    /// Occurs when the execution of the agent results in an exception.
    [<CLIEvent>]
    member x.Error = mbox.Error

    /// Like PostAndReply, but returns None if no reply within the timeout period.
    member x.TryPostAndReply(buildMessage, ?timeout) = 
        mbox.TryPostAndReply(buildMessageHelper buildMessage, ?timeout = timeout)

    /// Posts a message to the message queue of the MailboxProcessor, asynchronously.
    member x.Post(m:'a) = mbox.Post(Data m)

    /// Notifies the Mailbox to shutdown, further messages will not be processed and the Finished event will be triggered.
    member x.PostFinish(m) = mbox.Post(DataFinished)

    /// Posts a message to an agent and await a reply on the channel, synchronously.
    member x.PostAndReply(buildMessage, ?timeout) = 
        mbox.PostAndReply(buildMessageHelper buildMessage, ?timeout = timeout)

    /// Like PostAndAsyncReply, but returns None if no reply within the timeout period.
    member x.PostAndTryAsyncReply(buildMessage, ?timeout) = 
        mbox.PostAndTryAsyncReply(buildMessageHelper buildMessage, ?timeout = timeout)

    /// Posts a message to an agent and await a reply on the channel, asynchronously.
    member x.PostAndAsyncReply(buildMessage, ?timeout) = 
        mbox.PostAndAsyncReply(buildMessageHelper buildMessage, ?timeout=timeout)
