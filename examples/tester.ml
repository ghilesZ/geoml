module type Test = sig
  type t  
  val title : string
  val frame : t -> unit
  val size_x : float
  val size_y : float
  val new_val : unit -> t
end

module Make(T:Test) = struct
  let clear () = Drawing.fill_screen Graphics.white

  let handler status =
    let open Graphics in
    if status.key = 'r' then begin
      clear ();
      T.new_val () |> T.frame
    end
      
  let loop state = 
    Graphics.loop_at_exit [Graphics.Key_pressed] handler
      
  let doit () =
    Random.self_init ();
    Drawing.open_graph T.size_x T.size_y T.title;
    T.new_val () |> T.frame;
    loop ()
end
