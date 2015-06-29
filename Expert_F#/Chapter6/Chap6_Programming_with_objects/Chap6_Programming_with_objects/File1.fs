open System.Collections.Generic
open System.Drawing

let last_update_date = "2015/06/29"

/// Two-dimensional vectors
type Vector2D_type =
    {DX : float; DY : float}

    // instance property : Use as v.Length
    member v.Length = sqrt(v.DX * v.DX + v.DY * v.DY)
    member v.Length_with_side_effect = 
        printfn "Computing length!"
        sqrt(v.DX * v.DX + v.DY * v.DY)
    // instance methods : Use as v.Scale(2.5)
    member v.Scale(k) = {DX = k * v.DX; DY = k * v.DY}
    member v.ShiftX(x) = {v with DX = v.DX + x}
    member v.ShiftY(y) = {v with DY = v.DY + y}
    // For below example, conventional way to write is : v.ShiftXY(x, y) = ...
    member v.ShiftXY x y = {v with DX = v.DX + x; DY = v.DY + y}
    // static property : Use as Vector2D.Zero(will return {DX = 0.0; DY = 0.0})
    static member Zero = {DX = 0.0; DY = 0.0}
    // static methods : Use as Vector2D.ConstX(0.5) (will return {Dx = 0.5; DY = 0.0})
    static member ConstX(dx) = {DX = dx; DY = 0.0}
    static member ConstY(dy) = {DX = 0.0; DY = dy}



/// A type of binary trees, generic in the type of values carried at nodes and tips
type Tree<'T> =
    | Node of 'T * Tree<'T> * Tree<'T>
    | Tip

    // Compute the number of values in the tree
    member t.Size =
        match t with
        | Node(_, l, r) -> 1 + l.Size + r.Size
        | Tip -> 0



/// Vector2D using class type.
type Vector2D (dx : float, dy : float) = // This line shows primary constructor.
    
    let len = sqrt(dx * dx + dy * dy) // Computation is performed whenever class is constructed.

    member v.DX = dx
    member v.DY = dy
    member v.Length = len
    member v.Scale(k) = Vector2D(k * dx, k * dy)
    member v.ShiftX(x) = Vector2D(dx = dx + x, dy = dy)
    member v.ShiftY(y) = Vector2D(dx = dx, dy = dy + y)
    member v.ShiftXY(x, y) = Vector2D(dx = dx + x, dy = dy + y)
    static member Zero = Vector2D(dx = 0.0, dy = 0.0)
    static member OneX = Vector2D(dx = 1.0, dy = 0.0)
    static member OneY = Vector2D(dx = 0.0, dy = 1.0)
    static member (+) (v1 : Vector2D, v2 : Vector2D) =
        Vector2D(v1.DX + v2.DX, v1.DY + v2.DY)
    static member (-) (v1 : Vector2D, v2 : Vector2D) =
        Vector2D(v1.DX - v2.DX, v1.DY - v2.DY)



/// Vectors whose length is checked to be close to length one.
type UnitVector2D(dx, dy) =
    
    let tolerance = 0.000001

    let length = sqrt(dx * dx + dy * dy)

    do if abs (length - 1.0) >= tolerance then failwith "not a unit vector"

    member v.DX = dx
    member v.DY = dy

    new() = UnitVector2D(1.0, 0.0)



/// SparseVector with indexer property.
type SparseVector(items : seq<int * float>) =
    let elems = new SortedDictionary<_, _>()
    do items |> Seq.iter (fun (k, v) -> elems.Add(k, v))
    member t.Item 
        with get(idx) =
            if elems.ContainsKey(idx) then 
                elems.[idx] 
            else 
                0.0



/// Using Named and Optional Arguments
type LabelInfo (?text : string, ?font : Font) =
    let text = defaultArg text ""
    let font = match font with
                | None -> new Font (FontFamily.GenericSansSerif, 12.0f)
                | Some v -> v
    member x.Text = text
    member x.Font = font
    static member Create(?text, ?font) = new LabelInfo(?text = text, ?font = font)



/// Adding Method Overloading
/// Interver(lo, hi) represents the range of numbers from lo to hi,
/// but not including either lo or hi.
type Interval(lo, hi) =
    member r.Lo = lo
    member r.Hi = hi
    member r.IsEmpty = hi <= lo
    member r.Contains(v) = lo < v && v < hi
    static member Empty = Interval(0.0, 0.0)
    static member Span (r1 : Interval, r2 : Interval) =
        if r1.IsEmpty then 
            r2
        else if r2.IsEmpty then r1
        else 
            Interval (min r1.Lo r2.Lo, max r1.Hi r2.Hi)
    static member Span(ranges : seq<Interval>) =
        Seq.fold (fun r1 r2 -> Interval.Span(r1, r2)) Interval.Empty ranges


            
type MutableVector2D (dx : float, dy : float) =
    let mutable currDX = dx
    let mutable currDY = dy

    member vec.DX with get() = currDX and set v = currDX <- v
    member vex.DY with get() = currDY and set v = currDY <- v

    member vec.Length
        with get() = sqrt (currDX * currDX + currDY * currDY)
        and set len =
            let theta = vec.Angle
            currDX <- cos theta * len
            currDY <- sin theta * len

    member vec.Angle
        with get() = atan2 currDY currDX
        and set theta =
            let len = vec.Length
            currDX <- cos theta * len
            currDY <- sin theta * len



type IntegerMatrix (rows : int, cols : int) =
    let elems = Array2D.zeroCreate<int> rows cols

    member t.Item
        with get (idx1, idx2) = elems.[idx1, idx2]
        and set (idx1, idx2) v = elems.[idx1, idx2] <- v



/// Using Optional Property Settings, declaring Auto-Properties
type LabelInfoWithPropertySetting() =
    let mutable text = ""
    let mutable font = new Font(FontFamily.GenericSansSerif, 12.0f)
    member x.Text with get() = text and set v = text <- v
    member x.Font with get() = font and set v = font <- v
    
    // Auto-Properties - To use, commentize above two lines.
    //member val Text = "" with get, set
    //member val Font = new Font(FontFamily.GenericSansSerif, 12.0f) with get, set



/// Getting Started with Object Interface Types
/// Defining New Object Interface Types
type IShape =
    abstract Contains : Point -> bool
    abstract BoundingBox : Rectangle

/// Implementing Object Interface Types Using Object Expressions
let circle (center : Point, radius : int) =
    {new IShape with
        
        member x.Contains(p : Point) =
            let dx = float32 (p.X - center.X)
            let dy = float32 (p.Y - center.Y)
            sqrt(dx * dx + dy * dy) <= float32 radius
            
        member x.BoundingBox =
            Rectangle (center.X - radius, center.Y - radius, 2 * radius + 1, 2 * radius + 1)}

let square (center : Point, side : int) =
    { new IShape with
        member x.Contains(p : Point) =
            let dx = p.X - center.X
            let dy = p.Y - center.Y
            abs(dx) < side / 2 && abs(dy) < side / 2
        member x.BoundingBox =
            Rectangle(center.X - side, center.Y - side, side * 2, side * 2)}

/// Implementing Object Interface Types Using Concrete Types
type MutableCircle() =
    member val Center = Point(x = 0, y = 0) with get, set
    member val Radius = 10 with get, set

    member c.Perimeter = 2.0 * System.Math.PI * float c.Radius

    interface IShape with
        member c.Contains(p : Point) =
            let dx = float32 (p.X - c.Center.X)
            let dy = float32 (p.Y - c.Center.Y)
            sqrt (dx * dx + dy * dy) <= float32 c.Radius

        member c.BoundingBox =
            Rectangle (c.Center.X - c.Radius, c.Center.Y - c.Radius, 2 * c.Radius + 1, 2 * c.Radius + 1)



/// More Techniques to Implement Objects
/// Combining Object Expressions and Function Parameters
/// An object interface type that consumes characters and strings
type ITextOutputSink =
    /// When implemented, writes one Unicode string to the sink
    abstract WriteChar : char -> unit
    /// When implemented, writes one Unicode string to the sink
    abstract WriteString : string -> unit

/// Returns an object that implements ITextOutputSink by using writeCharFunction.
let simpleOutputSink writeCharFunction =
    {new ITextOutputSink with
        member x.WriteChar(c) = writeCharFunction c
        member x.WriteString(s) = s |> String.iter x.WriteChar}

let stringBuilderOutputSink (buf : System.Text.StringBuilder) =
    simpleOutputSink(fun c -> buf.Append(c) |> ignore)

printfn "This is Practice code for 'Expert F# 3.0, Chapter 6 - Programming With Objects'"
printfn "Last Updated : %s" last_update_date