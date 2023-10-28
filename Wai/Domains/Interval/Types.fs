module Wai.Domains.Interval.Types

type Number =
  | MinusInf
  | Num of int
  | PlusInf

  static member (+)(x, y) =
    match x, y with
    | Num l, Num r -> Num(l + r)

    | Num _, PlusInf
    | PlusInf, Num _ -> PlusInf

    | Num _, MinusInf
    | MinusInf, Num _ -> MinusInf

    | PlusInf, PlusInf -> PlusInf
    | MinusInf, MinusInf -> MinusInf
    | _ -> failwith "sum operands not supported"

  static member (-)(x, y) =
    match x, y with
    | Num l, Num r -> Num(l - r)

    | Num _, PlusInf -> MinusInf
    | Num _, MinusInf -> PlusInf

    | MinusInf, Num _ -> MinusInf
    | MinusInf, PlusInf -> MinusInf

    | PlusInf, Num _ -> PlusInf
    | PlusInf, MinusInf -> PlusInf
    | _ -> failwith "minus operands not supported"

  static member (*)(x, y) =
    match x, y with
    | Num l, Num r -> Num(l * r)

    | _, Num z
    | Num z, _ when z = 0 -> Num 0 // 0 x _ = 0

    | Num n, PlusInf
    | PlusInf, Num n -> if n < 0 then MinusInf else PlusInf

    | Num n, MinusInf
    | MinusInf, Num n -> if n < 0 then PlusInf else MinusInf

    | MinusInf, MinusInf
    | PlusInf, PlusInf -> PlusInf
    | PlusInf, MinusInf
    | MinusInf, PlusInf -> MinusInf

  static member (/)(x, y) =
    match x, y with
    | Num num, Num den ->
      match num, den with
      | 0, _ -> Num 0
      | _, 0 -> if num < 0 then MinusInf else PlusInf
      | _ -> Num(num / den)
    | Num _, PlusInf
    | Num _, MinusInf -> Num 0 // n / (+-Inf) = 0
    | PlusInf, Num n -> if n >= 0 then PlusInf else MinusInf
    | MinusInf, Num n -> if n >= 0 then MinusInf else PlusInf
    | _ -> Num 0 // (+-Inf) / (+-Inf) = 0

  override this.ToString() =
    match this with
    | PlusInf -> "+\u221E"
    | Num n -> n.ToString()
    | MinusInf -> "-\u221E"

type Interval =
  | Range of Number * Number
  | Bottom

  static member (~-) x =
    match x with
    | Range(n1, n2) ->
      match n1, n2 with
      | Num a, Num b -> Range(Num -b, Num -a)
      | MinusInf, Num a -> Range(Num -a, PlusInf)
      | Num a, PlusInf -> Range(MinusInf, Num -a)
      | _ -> Range(MinusInf, PlusInf) // [+-Inf, +-Inf] = [-Inf, +Inf] Always sound
    | Bottom -> Bottom

  static member (+)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) -> Range(a + c, b + d)
    | _ -> Bottom

  static member (-)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) -> Range(a - d, b - c)
    | _ -> Bottom

  static member (*)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) ->
      let ac = a * c
      let ad = a * d
      let bc = b * c
      let bd = b * d

      let min = List.min [ ac; ad; bc; bd ]
      let max = List.max [ ac; ad; bc; bd ]
      Range(min, max)
    | _ -> Bottom

  static member union x y =
    match x, y with
    | Range(a, b), Range(c, d) -> Range(min a c, max b d)
    | Bottom, _ -> y
    | _, Bottom -> x

  static member intersect x y =
    match x, y with
    | Range(a, b), Range(c, d) ->
      let lower = max a c
      let higher = min b d
      if lower <= higher then Range(lower, higher) else Bottom
    | _ -> Bottom

  static member (/)(x, y) =
    match x, y with
    | Range _, Range(Num 0, Num 0) -> Bottom
    | Range(a, b), Range(c, d) when Num 1 <= c ->
      let ac = a / c
      let ad = a / d
      let bc = b / c
      let bd = b / d

      let min = List.min [ ac; ad; bc; bd ]
      let max = List.max [ ac; ad; bc; bd ]
      Range(min, max)
    | Range _, Range(_, d) when d <= Num -1 -> -x / -y // [-b, -a] / [-d, -c] if d <= 0
    | Range _, Range(_, d) when d <= Num -1 ->
      let t1 = x / Interval.intersect y (Range(Num 1, PlusInf))
      let t2 = x / Interval.intersect y (Range(MinusInf, Num -1))
      Interval.union t1 t2
    | _ -> Bottom

  override this.ToString() =
    match this with
    | Range(l, r) -> $"[{l.ToString()}, {r.ToString()}]"
    | Bottom -> "\u22A5"
