type t

type word
(** A single cell of data on the stack *)

type block
(** Opaque block of [word]s from the stack. *)

val empty_block : block
(** Zero-sized [block]. *)

type value = int

val show_value : value -> string
val vals_of_block : block -> value list

val make : ret:int -> arg:block -> t
(** Creates a new fiber of a given return size, and with a block of values
    positioned in calling-convention order at the top of the stack. *)

val pop : t -> word
(** Pops an word off the fiber's stack. *)

val pop_int : t -> int
(** Pops an integer off the fiber's stack. *)

val pop_block : t -> int -> block
(** [pop_block fiber n] pops a [block] of n words off the top of the stack. *)

val in_place_int : t -> (int -> int) -> unit
(** Modifies an integer in-place on the top of the fiber's stack. *)

val push : t -> Vm_op.locator -> unit
(** Pushes a value given by the locator onto the stack. *)

val push_int : t -> int -> unit
(** Pushes an integer onto the top of the fiber's stack. *)

val push_zeroed : t -> int -> unit
(** [push_zeroed fiber n] pushes n zero-ints onto the top of the stack. *)

val push_block : t -> block -> unit
(** Pushes a [block] of words onto the top of the stack. *)

val store : t -> int -> unit
(** [store fiber fp_offset] stores the word on the top of the stack into the
    frame pointer offset given by [fp_offset]. *)

val sp_add : t -> int -> unit
(** Advances the stack pointer by an amount.
    Can be used to reserve space on the stack. *)

val sp_sub : t -> int -> unit
(** Subtracts the stack pointer by an amount. *)

val setup_new_frame : t -> pc:int -> unit
(** Sets up a new call frame, given the program counter of the last call frame.
    Expects all arguments to already be set up per the calling convention. *)

val restore_old_frame : t -> [ `Pc of int | `Done of block ]
(** Restores the old call frame, returning its program counter, or `Done if the
    fiber has reached the end of the first frame.
    Must happen with the frame's stack restored to the current frame's start,
    via e.g. [reset_to_fp]. *)

val reset_to_fp : t -> unit
(** Restores the stack pointer to the current frame pointer. *)

val debug_fiber : t -> string
