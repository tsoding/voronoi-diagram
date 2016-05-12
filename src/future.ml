open Unix;;
open Sys;;
open Marshal;;

module type FutureConfig =
  sig
    val process_limit : int
  end

module type F =
  sig
    type 'a future

    (* future f x executes f x in a separate thread *)
    val future : ('a -> 'b) -> 'a -> 'b future

    (* block and return the result of the computation when it completes *)
    val force : 'a future -> 'a
  end

module Make (Config: FutureConfig): F =
  struct
    type 'a future = {t : Thread.t option ref; pid : int ref; value : 'a option ref};;

    (* To generate a future, we use unix processes. Pipes are used to
       handle parent-child communication. The child process handles the
       computation and outputs the result to a shared file descriptor,
       to which the parent later reads. *)
    let future (f:'a -> 'b) (x:'a) : 'b future =
      (* establish future to be placed inside closure of sigchld *)
      let future = {t=ref None; pid=ref 0; value=ref None} in
      (* open pipe for communication between processes *)
      let (fd_in, fd_out) = pipe () in
      (* function writes result to the output of pipe () *)
      let write_result f =
        (* let _ = waitpid [] !(f.pid) in *)
        let result : 'b = Marshal.from_channel (in_channel_of_descr fd_in) in
        f.value := Some result in
      match fork () with
      (* for child process, write result to fd_out *)
      | 0 -> (
        close fd_in;
        let result = f x in
        let output = out_channel_of_descr fd_out in
        Marshal.to_channel output result [];
        Pervasives.exit 0)
      (* for parent process, set process id *)
      | id -> (
        close fd_out;
        future.pid := id;
        future.t := Some (Thread.create write_result future);
        future)
    ;;

    (* Force waits for the child thread and then reports the future's
   value. *)
    let force (f:'a future) : 'a =
      match !(f.t) with
        Some t -> (Thread.join t;
                   match !(f.value) with
                   | Some v -> v
                   | None -> failwith "waited for thread, but no result")
      | None -> failwith "no thread available for future"
    ;;
  end

module Default: F = Make(struct let process_limit = max_int end)

                        khooy
