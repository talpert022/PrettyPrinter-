signature PrettyPrint =
sig
    type Layout
    type Doc

    (* A simple doc showing a string *)
    val text : string -> Doc

    (* Vertical compositon *)
    val $$ : Doc * Doc -> Doc

    (* Horizontal composition *)
    val <> : Doc * Doc -> Doc

    (* Combining a list of Docs horizonatally or vertically depending on the context *)
    val sep : Doc list -> Doc

    (* Indents a doc a certain number of spaces *)
    val nest : int -> Doc -> Doc

    (* Creates a union between two documents *)
    val union: Doc -> Doc -> Doc

    (* Determines the best layout for a doc to be printed *)
    val best : int -> int -> Doc -> Layout

    (* Mapping layouts to appropriate strings*)
    val layout : int -> Layout -> string

    (* prints a Doc directly to the terminal using best and layout functions *)
    val printDoc : int*int*int -> Doc -> unit

end
