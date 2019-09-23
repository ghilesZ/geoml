(** Triangles manipulation *)
module Make: functor
  (A:Arith.T)
  (Aff:Signatures.Affine_Sig)
  (P:Signatures.Point_Sig   with type arith = A.t and type affine = Aff.t)
  (V:Signatures.Vector_sig  with type arith = A.t and type point = P.t)
  (L:Signatures.Line_sig    with type arith = A.t and type point = P.t)
  (S:Signatures.Segment_sig with type point = P.t and type line = L.t)
-> Signatures.Triangle_sig
