module cp2021t

open Cp


// (1) Datatype definition -----------------------------------------------------
//data BTree a = Empty | Node(a, (BTree a, BTree a)) deriving Show
type BTree<'a> = Empty | Node of 'a * (BTree<'a> * BTree<'a>)

let inBTree a = either (konst Empty) Node x 

let outBTree a = 
    match a with 
        | Empty -> Left ()
        | (Node(a , (t1 , t2))) -> Right (a , (t1 , t2))

// (2) Ana + cata + hylo -------------------------------------------------------
let recBTree g = baseBTree id g

let cataBTree g a = (g << (recBTree (cataBTree g)) << outBTree) a

let anaBTree g a = (inBTree << (recBTree (anaBTree g) ) << g) a

let hyloBTree h g = cataBTree h << anaBTree g

let baseBTree f g = id -|- (f >< (g >< g))

// (3) Map ---------------------------------------------------------------------
(*
    instance Functor BTree
            where fmap f = cataBTree ( inBTree . baseBTree f id )
*) 

// (4) Examples ----------------------------------------------------------------
// (4.1) Inversion (mirror) ----------------------------------------------------
let invBTree a = cataBTree (inBTree << (id -|- (id >< swap))) a

// (4.2) Counting --------------------------------------------------------------

let countBTree a = cataBTree (either (konst 0) (succ << (uncurry (+)) << p2)) a 

// (4.3) Serialization ---------------------------------------------------------
let inordt = cataBTree inord    

let inord a = either nil join

let join (x,(l,r)) = l @ [x] @ r

let preordt = cataBTree preord

let preord = (either nil preord_g)

let preord_g (x,(l,r)) = x :: l @ r

let postordt = cataBTree (either nil postordt_g)

let postordt_g (x,(l,r)) = l @ r @ [x]

// (4.4) Quicksort -------------------------------------------------------------
let qSort a = hyloBTree inord qsep

//let qsep [] = Left ()
//let qsep list = Right (list.Head part (fun a -> a < list.Head) list.Tail) 

let qsep list = 
    match list with
        | list.isEmpty -> Left ()
        | _            -> Right (list.Head part ( fun a -> a < list.Head)  list.Tail)

let part p [] = ([],[])
let rec part p list =
    match list.Length with
        | p list.Head -> let (s,l) = part p list.Tail in (list.Head @ s, l) 
        | otherwise   -> let (s,l) = part p list.Tail in (s, list.Head @ l)

// (4.5) Traces ----------------------------------------------------------------

let traces = cataBTree (Either (konst [[]]) tunion)

let tunion (a, (l,r)) = union (map (fun x -> a @ x) l) (map (fun x -> a @ x) r)

// (4.6) Towers of Hanoi -------------------------------------------------------

let hanoi = hyloBTree present strategy

// where

let present = inord

let strategy d n = 
    match n with
        | 0     -> Left ()
        | _     -> Right ((n,d), ((not d,n), (not d,n)))

// (5) Depth and balancing (using mutual recursion) --------------------------

let balBTree = p1 . baldepth

let depthBTree = p2 . baldepth

let baldepth = cataBTree depbal

let depbal = Either (konst (True, 1)) ( (fun (a,((b1,b2),(d1,d2))) -> (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2)) . ( id  ><  (fun ((b1,d1),(b2,d2)) -> ((b1,b2),(d1,d2))))

// (6) Going polytipic -------------------------------------------------------

// (7) Zipper ----------------------------------------------------------------
type Deriv<'a> = Dr Bool 'a * (Btree<'a>)

type Zipper a = [ Deriv a ]

let rec plug list t =
    match list with
        | list.isEmpty  -> t
        | _             -> match list.Head with 
                            | Dr (False a l) -> Node (a, (plug list.Tail t, l))
                            | Dr (True a r)  -> Node (a, (r, plug list.Tail t))