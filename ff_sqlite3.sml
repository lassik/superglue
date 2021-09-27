(*! Generator: superglue *)

val open : string -> ff.sqlite3.db ff.result;
val prepare : ff.sqlite3.db * string -> unit ff.result;
val errstr : nat -> string ff.result;
val errmsg : ff.sqlite3.db -> string ff.result;
val set_last_insert_rowid : ff.sqlite3.db * int -> unit ff.result;
val last_insert_rowid : ff.sqlite3.db -> int ff.result;
val bind_parameter_count : ff.sqlite3.stmt -> nat ff.result;
val bind_parameter_index : ff.sqlite3.stmt * string -> nat ff.result;
val bind_null : ff.sqlite3.stmt * nat -> unit ff.result;
val bind_text : ff.sqlite3.stmt * nat * string -> unit ff.result;
val bind_int64 : ff.sqlite3.stmt * nat * int -> unit ff.result;
val bind_double : ff.sqlite3.stmt * nat * real -> unit ff.result;
