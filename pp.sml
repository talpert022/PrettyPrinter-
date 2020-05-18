structure prettyprint : PrettyPrint =
struct

    (* Rule for Docs:
          E -> Doc, U -> Union, M -> Manifest
          (rule 1) E ::= U | Nest N E
                   U ::= M | U u U
                   M ::= text S | text S $$ E
    *)

    (* best :: Doc -> Layout,
       Layout is a Doc not involving a Union or Empty
    *)

    datatype Doc =
        Empty
      | Text of string
      | Above of string * Doc
      | Union of Doc * Doc
      | Nest of int * Doc

    (* A layout is a Doc not involving a Union or Empty *)
    datatype Layout =
        Text' of string
      | Above' of string * Layout
      | Nest' of int * Layout

    (* printing a simple string*)
    fun text (s : string) : Doc =
        Text(s)

    (* Indents a doc a certain number of spaces *)
    fun nest (k : int)(x : Doc) : Doc =
        Nest(k,x)

    (* Creates a union between two documents *)
    fun union (d1 : Doc) (d2 : Doc) : Doc =
        Union(d1,d2)

    infixr $$
    (* Vertical compositon *)
    fun (d1 : Doc) $$ (d2 : Doc) : Doc =
        case d1 of
            Text(s) => Above(s,d2)
          | Above(s,x) => Above(s, x $$ d2)
          | Nest(k,x) => Nest(k, x $$ Nest(~k, d2))
          | Union(x1,x2) => Union(x1 $$ d2, x2 $$ d2)

    infixr <>
    (* Horizontal composition*)
    fun (d1 : Doc) <> (d2 : Doc) : Doc =
        case (d1,d2) of
            (Text(s), Text(t)) => Text(s ^ t)
          | (Text(s), Above(t,x)) => Above(s ^ t, Nest(size s, x))
          | (Text(s), Nest(k,x)) => Text(s) <> x
          | (Text(s), Union(x,y)) => Union(Text(s) <> x, Text(s) <> y)
          | (Above(s,x), y) => Above(s, x <> y)
          | (Nest(k,x), y) => Nest(k, x <> y)
          | (Union(x,y), z) => Union (x <> z, y <> z)

    (* Combining a list of Docs horizonatally or vertically depending on the context *)
    fun sep (d : Doc list) : Doc =
        case d of
            [x] => x
          | Nest(k,x) :: xs => Nest(k, sep(x :: map(fn doc => Nest(~k, doc)) xs ))
      (*  | xs => fit (foldr (fn (d1,d2) => d1 <> Text(" ") <> d2) Text("") xs)
                  'u' foldr(fn (d1,d2) => d1 $$ d2) Text("") xs
                      where Empty 'u' y => y
                            x 'u' y => Union(x,y) *)

    (* Helper function for sep that: more info *)
    fun fit (d : Doc) : Doc =
        case d of
          Text(s) => Text(s)
        | Above(s,x) => Empty
        | Nest(k,x) => ( case fit x of
                            Empty => Empty
                          | _ => Nest(k,x))
        | Union(x,y) => fit x

    (* Helper function for best that determines the best way to layout a union *)
    fun nicest (w : int)(r : int)(l1 : Layout)(l2 : Layout) : Layout =
        let
            fun shorter (s : string) (n : int) : bool = size s <= n
            fun firstline (l : Layout) : string =
                case l of
                    Text'(s) => s
                  | Above'(s,x) => s
        in
            if shorter (firstline l1) (Int.min (w,r)) then l1 else l2
        end

    (* Determines the best layout for a doc to be printed, given a width of the page and ribbon width of the line *)
    fun best (w : int)(r : int)(d : Doc) : Layout =
        case d of
            Text(s) => Text'(s)
          | Above(s,x) => Above'(s, best w r x)
          | Nest (k,x) => Nest'(k, best (w-k) r x)
          | Union(x,y) => nicest w r (best w r x) (best w r y)

    (* Helper function for layout that indents a number of spaces *)
    fun indent (k : int)(s : string) : string =
        case k of
            0 => s ^ "\n"
          | _ => if k>8 then "\t" ^ indent (k-8) s
                        else " " ^ indent (k-1) s

    (* Mapping layouts to appropriate strings *)
    fun layout (k : int)(l : Layout) : string =
        case l of
            Text'(s) => indent k s
          | Above'(s,x) => indent k s  ^  layout k x
          | Nest'(k', x) => layout (k + k') x

    (* prints a Doc directly to the terminal using best and layout functions *)
    fun printDoc (w : int, r : int, k : int)(d : Doc) : unit =
        let
            val printSTR = layout(k)(best w r d)
        in
            print("\n" ^ printSTR ^ "\n")
        end

end
