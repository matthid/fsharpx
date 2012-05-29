﻿/// vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.Vector

open FSharpx
open System.Threading

let is64BitProcess = System.IntPtr.Size = 8
let blockSizeShift = 5 // TODO: what can we do in 64Bit case?
let blockSize = 1 <<< blockSizeShift
let blockIndexMask = blockSize - 1

type Node(thread,array:obj[]) =
    let thread = thread
    new() = Node(ref null,Array.create blockSize null)
    with
        static member InCurrentThread() = Node(ref Thread.CurrentThread,Array.create blockSize null)
        member this.Array = array
        member this.Thread = thread
        member this.SetThread t = thread := t

type TransientVector<'a> (count,shift:int,root:Node,tail:obj[]) =
    let mutable count = count
    let mutable root = root
    let mutable tail = tail
    let mutable shift = shift

    new() = TransientVector<'a>(0,blockSizeShift,Node.InCurrentThread(),Array.create blockSize null)
    
    with
        member internal this.EnsureEditable(node:Node) =
            if node.Thread = root.Thread then node else
            Node(root.Thread,Array.copy node.Array)

        member internal this.NewPath(level,node:Node) =
            if level = 0 then node else
            let ret = Array.create blockSize null
            ret.[0] <- this.NewPath(level - blockSizeShift,node) :> obj
            Node(node.Thread,ret)

        member internal this.PushTail(level,parent:Node,tailnode) =
            //if parent is leaf, insert node,
            // else does it map to an existing child? -> nodeToInsert = pushNode one more level
            // else alloc new path
            //return  nodeToInsert placed in copy of parent
            let parent = this.EnsureEditable parent
            let subidx = ((count - 1) >>> level) &&& blockIndexMask
            let ret = parent

            let nodeToInsert =
                if level = blockSizeShift then tailnode else

                let child = parent.Array.[subidx]
                if child <> null then
                    this.PushTail(level-blockSizeShift,child :?> Node,tailnode)
                else
                    this.NewPath(level-blockSizeShift,tailnode)

            ret.Array.[subidx] <- nodeToInsert :> obj
            ret

        member internal this.ArrayFor<'a> i =
            if i >= 0 && i < count then
                if i >= this.TailOff() then tail else
                    let mutable node = root
                    let mutable level = shift
                    while level > 0 do
                        let pos = (i >>> level) &&& blockIndexMask
                        node <- node.Array.[pos] :?> Node
                        level <- level - blockSizeShift

                    node.Array
            else raise Exceptions.OutOfBounds


        member internal this.EditableArrayFor<'a> i =
            if i >= 0 && i < count then
                if i >= this.TailOff() then tail else
                    let mutable node = root
                    let mutable level = shift
                    while level > 0 do
                        let pos = (i >>> level) &&& blockIndexMask
                        node <- this.EnsureEditable(node.Array.[pos] :?> Node)
                        level <- level - blockSizeShift

                    node.Array
            else raise Exceptions.OutOfBounds

        member this.nth i =
                this.EnsureEditable()
                let node = this.ArrayFor i
                node.[i &&& blockIndexMask] :?> 'a

        member this.conj<'a> (x:'a) =
            this.EnsureEditable()

            //room in tail?
            if count - this.TailOff() < blockSize then
                tail.[count &&& blockIndexMask] <- x :> obj
            else
                //full tail, push into tree
                let tailNode = Node(root.Thread,tail)
                let newShift = shift
                let newTail = Array.create blockSize null
                newTail.[0] <- x :> obj

                //overflow root?
                let newRoot = 
                    if (count >>> blockSizeShift) > (1 <<< shift) then
                        let newRoot = Node(root.Thread,Array.create blockSize null)
                        newRoot.Array.[0] <- root :> obj
                        newRoot.Array.[1] <- this.NewPath(shift,tailNode) :> obj
                        shift <- shift + blockSizeShift
                        newRoot
                    else
                        this.PushTail(shift,root,tailNode)

                tail <- newTail
                root <- newRoot

            count <- count + 1
            this

        member internal this.doAssoc(level,node:Node,i,x) =
            let node = this.EnsureEditable(node)
            let ret = node
            if level = 0 then 
                ret.Array.[i &&& blockIndexMask] <- x :> obj 
            else
                let subidx = (i >>> level) &&& blockIndexMask
                ret.Array.[subidx] <- this.doAssoc(level - blockSizeShift, node.Array.[subidx] :?> Node, i, x) :> obj
            ret

        member this.assocN<'a>(i,x:'a) : TransientVector<'a> =
            this.EnsureEditable()
            if i >= 0 && i < count then
                if i >= this.TailOff() then
                    tail.[i &&& blockIndexMask] <- x :> obj
                    this
                else
                    root <- this.doAssoc(shift, root, i, x)
                    this
            elif i = count then
                this.conj x
            else raise Exceptions.OutOfBounds

        member internal this.PopTail(level,node:Node) : Node =
            let node = this.EnsureEditable(node)
            let subidx = ((count-2) >>> level) &&& blockIndexMask
            if level > blockSizeShift then
                let newchild = this.PopTail(level - blockSizeShift, node.Array.[subidx] :?> Node)
                if newchild = Unchecked.defaultof<Node> && subidx = 0 then Unchecked.defaultof<Node> else
                let ret = node
                ret.Array.[subidx] <- newchild  :> obj
                ret

            elif subidx = 0 then Unchecked.defaultof<Node> else

            let ret = node
            ret.Array.[subidx] <- null
            ret

        member this.pop() =
            this.EnsureEditable()
            if count = 0 then failwith "Can't pop empty vector" else
            if count = 1 then count <- 0; this else

            let i = count - 1
            if (i &&& blockIndexMask) > 0 then count <- count - 1; this else

            let newtail = this.EditableArrayFor(count - 2)

            let mutable newroot = this.PopTail(shift, root)
            let mutable newshift = shift
            if newroot = Unchecked.defaultof<Node> then
                newroot <- Node(root.Thread,Array.create blockSize null)

            if shift > blockSizeShift && newroot.Array.[1] = null then
                newroot <- this.EnsureEditable(newroot.Array.[0] :?> Node)
                newshift <- newshift - blockSizeShift

            root <- newroot
            shift <- newshift
            count <- count - 1
            tail <- newtail
            this

        member this.rangedIterator<'a>(startIndex,endIndex) : 'a seq =
            let i = ref startIndex
            let b = ref (!i - (!i % blockSize))
            let array = if startIndex < count then ref (this.ArrayFor !i) else ref null

            seq {
                while !i < endIndex do
                    if !i - !b = blockSize then
                        array := this.ArrayFor !i
                        b := !b + blockSize

                    yield (!array).[!i &&& blockIndexMask] :?> 'a
                    i := !i + 1 
               }

        member this.persistent() : PersistentVector<'a> =
            this.EnsureEditable()
            root.SetThread null
            let l = count - this.TailOff()
            let trimmedTail = Array.init l (fun i -> tail.[i])
            PersistentVector(count, shift, root, trimmedTail,None)

        member internal this.EnsureEditable() =
            if !root.Thread = Thread.CurrentThread then () else
            if !root.Thread <> null then
                failwith "Transient used by non-owner thread"
            failwith "Transient used after persistent! call"

        member internal this.TailOff() =
            if count < blockSize then 0 else
            ((count - 1) >>> blockSizeShift) <<< blockSizeShift
        
        interface System.Collections.Generic.IEnumerable<'a> with
            member this.GetEnumerator () =
              this.rangedIterator(0,count).GetEnumerator()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
              (this.rangedIterator(0,count).GetEnumerator())
                :> System.Collections.IEnumerator

        interface IVector<'a> with
            member this.Item with get i = this.nth i
            member this.Conj x = this.conj x :> IVector<'a>
            member this.Pop() = this.pop() :> IVector<'a>
            member this.Peek() = if count > 0 then this.nth(count - 1) else failwith "Can't peek empty vector"
            member this.Count() = this.EnsureEditable(); count
            member this.AssocN(i,x) = this.assocN(i,x) :> IVector<'a>

and PersistentVector<[<EqualityConditionalOn>]'a> (count,shift:int,root:Node,tail:obj[],hashCode)  =
    let hashCode = ref hashCode
    let tailOff = 
        if count < blockSize then 0 else
        ((count - 1) >>> blockSizeShift) <<< blockSizeShift

    with
        static member Empty() = PersistentVector<'a>(0,blockSizeShift,Node(),[||],None)

        static member ofSeq(items:'a seq) =
            let mutable ret = TransientVector()
            for item in items do
                ret <- ret.conj item
            ret.persistent()

        override this.GetHashCode() =
            match !hashCode with
            | None ->
                let mutable hash = 1
                for x in this.rangedIterator(0,count) do
                    hash <- 31 * hash + Unchecked.hash x
                hashCode := Some hash
                hash
            | Some hash -> hash

        override this.Equals(other) =
            let v = this :> IVector<'a>
            match other with
            | :? IVector<'a> as y -> 
                if v.Count() <> y.Count() then false else
                if v.GetHashCode() <> y.GetHashCode() then false else
                Seq.forall2 (Unchecked.equals) this y
            | _ -> false

        member internal this.SetHash hash = hashCode := hash; this

        member internal this.NewPath(level,node:Node) =
            if level = 0 then node else
            let ret = Node(root.Thread,Array.create blockSize null)
            ret.Array.[0] <- this.NewPath(level - blockSizeShift,node) :> obj
            ret

        member internal this.PushTail(level,parent:Node,tailnode) =
            //if parent is leaf, insert node,
            // else does it map to an existing child? -> nodeToInsert = pushNode one more level
            // else alloc new path
            //return  nodeToInsert placed in copy of parent
            let subidx = ((count - 1) >>> level) &&& blockIndexMask
            let ret = Node(parent.Thread,Array.copy parent.Array)

            let nodeToInsert =
                if level = blockSizeShift then tailnode else

                let child = parent.Array.[subidx]
                if child <> null then
                    this.PushTail(level-blockSizeShift,child :?> Node,tailnode)
                else
                    this.NewPath(level-blockSizeShift,tailnode)

            ret.Array.[subidx] <- nodeToInsert :> obj
            ret

        member internal this.ArrayFor<'a> i =
            if i >= 0 && i < count then
                if i >= tailOff then tail else
                    let mutable node = root
                    let mutable level = shift
                    while level > 0 do
                        let pos = (i >>> level) &&& blockIndexMask
                        node <- node.Array.[pos] :?> Node
                        level <- level - blockSizeShift

                    node.Array
            else raise Exceptions.OutOfBounds

        member this.nth<'a> i : 'a =
            let node = this.ArrayFor i
            node.[i &&& blockIndexMask] :?> 'a

        member this.cons<'a> (x:'a) =
            let newHash = Some(31 * Unchecked.hash this + Unchecked.hash x)
            if count - tailOff < blockSize then
                let newTail = Array.append tail [|x:>obj|]
                PersistentVector<'a>(count + 1,shift,root,newTail,newHash )
            else
                //full tail, push into tree
                let tailNode = Node(root.Thread,tail)
                let newShift = shift

                //overflow root?
                if (count >>> blockSizeShift) > (1 <<< shift) then
                    let newRoot = Node(root.Thread,Array.create blockSize null)
                    newRoot.Array.[0] <- root :> obj
                    newRoot.Array.[1] <- this.NewPath(shift,tailNode) :> obj
                    PersistentVector<'a>(count + 1,shift + blockSizeShift,newRoot,[| x |],newHash)
                else
                    let newRoot = this.PushTail(shift,root,tailNode)
                    PersistentVector<'a>(count + 1,shift,newRoot,[| x |],newHash)

        member internal this.doAssoc(level,node:Node,i,x) =
            let ret = Node(root.Thread,Array.copy node.Array)
            if level = 0 then 
                ret.Array.[i &&& blockIndexMask] <- x :> obj 
            else
                let subidx = (i >>> level) &&& blockIndexMask
                ret.Array.[subidx] <- this.doAssoc(level - blockSizeShift, node.Array.[subidx] :?> Node, i, x) :> obj
            ret

        member this.assocN<'a>(i,x:'a) : PersistentVector<'a> =
            if i >= 0 && i < count then
                if i >= tailOff then
                    let newTail = Array.copy tail
                    newTail.[i &&& blockIndexMask] <- x :> obj
                    PersistentVector(count, shift, root, newTail,None)
                else
                    PersistentVector(count, shift, this.doAssoc(shift, root, i, x),tail,None)
            elif i = count then
                this.cons x
            else raise Exceptions.OutOfBounds

        member internal this.PopTail(level,node:Node) : Node =
            let subidx = ((count-2) >>> level) &&& blockIndexMask
            if level > blockSizeShift then
                let newchild = this.PopTail(level - blockSizeShift, node.Array.[subidx] :?> Node)
                if newchild = Unchecked.defaultof<Node> && subidx = 0 then Unchecked.defaultof<Node> else
                let ret = Node(root.Thread, Array.copy node.Array);
                ret.Array.[subidx] <- newchild  :> obj
                ret

            elif subidx = 0 then Unchecked.defaultof<Node> else

            let ret = new Node(root.Thread, Array.copy node.Array)
            ret.Array.[subidx] <- null
            ret

        member this.pop() =
            if count = 0 then failwith "Can't pop empty vector" else
            if count = 1 then PersistentVector<'a>.Empty() else

            //if(tail.length > 1)
            if count - tailOff > 1 then PersistentVector(count - 1, shift, root, tail.[0..(tail.Length-1)],None) else

            let newtail = this.ArrayFor(count - 2)

            let mutable newroot = this.PopTail(shift, root)
            let mutable newshift = shift
            if newroot = Unchecked.defaultof<Node> then
                newroot <- Node()

            if shift > blockSizeShift && newroot.Array.[1] = null then
                newroot <- newroot.Array.[0] :?> Node
                newshift <- newshift - blockSizeShift

            PersistentVector(count - 1, newshift, newroot, newtail,None)

        member this.rangedIterator<'a>(startIndex,endIndex) : 'a seq =
            let i = ref startIndex
            let b = ref (!i - (!i % blockSize))
            let array = if startIndex < count then ref (this.ArrayFor !i) else ref null

            seq {
                while !i < endIndex do
                    if !i - !b = blockSize then
                        array := this.ArrayFor !i
                        b := !b + blockSize

                    yield (!array).[!i &&& blockIndexMask] :?> 'a
                    i := !i + 1 
               }

        interface System.Collections.Generic.IEnumerable<'a> with
            member this.GetEnumerator () =
              this.rangedIterator(0,count).GetEnumerator()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
              (this.rangedIterator(0,count).GetEnumerator())
                :> System.Collections.IEnumerator

        interface IVector<'a> with
            member this.Item with get i = this.nth i
            member this.Conj x = this.cons x :> IVector<'a>
            member this.Pop() = this.pop() :> IVector<'a>
            member this.Count() = count
            member this.Peek() = if count > 0 then this.nth(count - 1) else failwith "Can't peek empty vector"

            member this.AssocN(i,x) = this.assocN(i,x) :> IVector<'a>

type 'a vector = IVector<'a>

/// Returns the number of items in the collection.
let inline count (vector:'a vector) : int = vector.Count()

let empty<'a> = PersistentVector<'a>.Empty() :> IVector<'a>

/// Returns the value at the index. If the index is out of bounds it throws an exception.
let inline nth<'a> i (vector:'a vector) : 'a = vector.[i]
 
/// Returns a new vector with the element 'added' at the end.   
let inline cons<'a> (x:'a) (vector:'a vector) = vector.Conj x

/// Returns the last element in the vector. If the vector is empty it throws an exception.
let inline peek<'a> (vector:'a vector) = vector.Peek()

/// Returns a new vector without the last item. If the collection is empty it throws an exception.
let inline pop<'a> (vector:'a vector) = vector.Pop()

/// Returns a new vector that contains the given value at the index. Note - index must be <= vector.Count.
let inline assocN<'a> i (x:'a) (vector:'a vector) : 'a vector = vector.AssocN(i,x)

let inline ofSeq (items:'a seq) = PersistentVector.ofSeq items :> IVector<'a>