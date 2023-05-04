(* The key control flow operations of Go are:

   * Cooperatively scheduled goroutines (coroutines)
   * Defer statements
   * Panics and recovers

   Reproduction of https://go.dev/blog/defer-panic-and-recover
 **)

module Go = struct
  type ('a, 'b) func = 'a -> 'b
  type _ Effect.t += Defer : (unit -> unit) -> unit Effect.t
                   | Yield : unit Effect.t
                   | Go : (unit ->  unit) -> unit Effect.t
                   | Recover : string option Effect.t

  exception Panic of string

  let panic : 'a. string -> 'a
    = fun s -> raise (Panic s)

  let recover : unit -> string option
    = fun () -> Effect.perform Recover

  let defer : (unit -> unit) -> unit
    = fun f -> Effect.perform (Defer f)

  let yield : unit -> unit
    = fun () -> Effect.perform Yield

  let go : (unit -> 'a) -> unit
    = fun g -> Effect.perform (Go (fun () -> ignore (g ())))

  module Scheduler = struct
    let run_next q =
      if Queue.is_empty q
      then ()
      else Queue.pop q ()

    let rec hsched q =
      let open Effect.Deep in
      { retc = (fun _ -> run_next q)
      ; exnc = raise (* TODO: what's the Go semantics of panic
                        propagation across asynchronous calls? *)
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Go g ->
           Some (fun (k : (a, _) continuation) ->
               Queue.push
                 (fun () ->
                   Effect.Deep.match_with g () (hsched q)) q;
               continue k ())
        | Yield ->
           Some (fun (k : (a, _) continuation) ->
               Queue.push (continue k) q;
               run_next q)
        | _ -> None) }
    and run : 'a. (unit -> 'a) -> unit option
      = fun f ->
      let f () = ignore (f ()) in
      ignore (Effect.Deep.match_with f () (hsched (Queue.create ())));
      None
  end

  let run = Scheduler.run

  module Func = struct
    type ('a, 'b) t = 'a -> 'b

    let rec run_defers ds =
      Stack.iter (fun f -> f ()) ds

    let hrecover s =
      let open Effect.Deep in
      { retc = (fun () -> false)
      ; exnc = raise
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Recover ->
           Some (fun (k : (a, _) continuation) ->
               ignore (continue k (Some s)); true
             )
        | _ -> None) }

    let hfunc () =
      let defers = Stack.create () in
      let open Effect.Deep in
      { retc = (fun x -> run_defers defers; Some x)
      ; exnc = (fun exn ->
        match exn with
        | Panic s ->
           if Effect.Deep.match_with run_defers defers (hrecover s)
           then None
           else raise exn
        | _ -> raise exn)
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Defer f ->
           Some (fun (k : (a, _) continuation) ->
               Stack.push f defers;
               continue k ()
             )
        | Recover ->
           Some (fun (k : (a, _) continuation) ->
               continue k None)
        | _ -> None) }

    let func : ('a -> 'b) -> ('a, 'b option) func
      = fun f x ->
      Effect.Deep.match_with f x (hfunc ())
  end

  let func = Func.func
end

let ex0 () =
  let open Go in
  func (fun () ->
      for i = 0 to 3 do
        defer (fun () -> Printf.printf "%d\n%!" i)
      done) ()

let ex1 : int ref -> unit option
  = fun r ->
  let open Go in
  func (fun () ->
      defer (fun () -> incr r)) ()

let ex2 () =
  let open Go in
  let rec g : int -> unit option
    = fun i ->
    func (fun i ->
        if i > 3
        then (Printf.printf "Panicking!\n%!";
              panic (Printf.sprintf "%d" i));
        defer (fun () ->
            Printf.printf "Defer in g %d\n%!" i);
        Printf.printf "Printing in g %d\n%!" i;
        ignore (g (i + 1))) i
  in
  let f =
    func (fun () ->
        defer (fun () ->
            let r = recover () in
            match r with
            | Some s ->
               Printf.printf "Recovered in f %s\n%!" s
            | None -> ());
        Printf.printf "Calling g.\n%!";
        ignore (g 0);
        Printf.printf "Returned normally from g.\n%!")
  in
  let main =
    func (fun () ->
        ignore (f ());
        Printf.printf "Returned normally from f.\n%!")
  in
  main ()

let ex3 () =
  let open Go in
  let f =
    func (fun () ->
        Printf.printf "H";
        yield ();
        Printf.printf "l";
        yield ();
        Printf.printf "W";
        yield ();
        Printf.printf "l";
        yield ();
        Printf.printf "\n%!")
  in
  let g =
    func (fun () ->
        Printf.printf "e";
        yield ();
        Printf.printf "o";
        yield ();
        Printf.printf "o";
        yield ();
        Printf.printf "d")
  in
  let h =
    func (fun () ->
        Printf.printf "l";
        yield ();
        Printf.printf " ";
        yield ();
        Printf.printf "r")
  in
  let main =
    func (fun () ->
        go f;
        go g;
        go h)
  in
  run main
