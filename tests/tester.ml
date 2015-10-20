module type Test = sig
  val title : string
  val frame : unit -> unit
  val size_x : float
  val size_y : float
end

module Make(T:Test) = struct
  let clear () = Drawing.fill_screen Graphics.white

  let handler status =
    let open Graphics in
    if status.key = 'r' then begin clear (); T.frame() end
      
  let loop state = 
    Graphics.loop_at_exit [Graphics.Key_pressed] handler
      
  let doit () =
    Drawing.open_graph T.size_x T.size_y T.title;
    T.frame ();
    loop ()
end
