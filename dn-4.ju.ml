# %% [markdown]
# # 4. domača naloga

# %% [markdown]
# Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike zafiksiramo tip `direction`:

# %%
type direction =
    | Left
    | Right

type state = string

# %% [markdown]
# ## Implementacija trakov

# %% [markdown]
# Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:
#
# - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
# - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
# - `read`, ki vrne znak pod glavo;
# - `write`, ki pod glavo zapiše dani znak;
# - `move`, ki glavo premakne v dano smer;
# - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter pod njim z `^` označi mesto glave.
#
# Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita nespremenjenega.
#
# Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato poskrbite tako za učinkovitost kot za preglednost kode.

# %%
module type TAPE =
    sig
        type t

        val make : string -> t
        val move : direction -> t -> t
        val read : t -> char
        val write : char -> t -> t
        val print : t -> unit
    end

module Tape : TAPE =
    struct
        (* Choosing the correct type has an impact on time efficiency. We can limit ourselves as follows: *)

        (* -`make` function has to read every character in the string so at best it is O(n) *)
        (* -`move` function has to move the tape by a single step so it should be O(1) *)
        (* -`read` function has to output a single character so it should be O(1) *)
        (* -`write` function has to replace a single character so it should be O(1) *)
        (* -`print` function has to read the entire tape so at best it is O(n) *)

        (* We see that our goal should be optimizing `move`, `read` and `write`, since the others are at least O(n). *)
        (* This means we need O(1) access to reading and writing of the head and O(1) to the characters next to it. *)

        (* Idea: Store the head seperately for easy reading and writing and have lists of characters to the *)
        (* left and right with the first character being the one next to the head *)

        (*type t = char list * char * char list*)
        type t =
            {
                left : char list;
                head : char;
                right : char list
            }

        let empty_tape : t =
            { left = []; head = ' '; right = [] }

        let make (s : string) : t =
            let str_to_list (s : string) : char list =
                List.init (String.length s) (String.get s)
            in
            match str_to_list (String.trim s) with
            | [] -> empty_tape
            | hd :: tl -> { empty_tape with head = hd; right = tl }

        let move (dir : direction) (tape : t) : t =
            let order (hd : char) ((dir_chars, other_chars) : char list * char list) : t =
                match dir with
                | Left -> { left = dir_chars; head = hd; right = other_chars }
                | Right -> { left = other_chars; head = hd; right = dir_chars }
            and trim_others (other_chars : char list) : char list =
                if tape.head = ' ' && other_chars = [] then other_chars
                else tape.head :: other_chars
            and (dir_chars, other_chars) : char list * char list =
                match dir with
                | Left -> (tape.left, tape.right)
                | Right -> (tape.right, tape.left)
            in
            match dir_chars with
            | [] -> order ' ' ([], trim_others other_chars)
            | hd' :: dir_chars' -> order hd' (dir_chars', trim_others other_chars)

        let read (tape : t) : char =
            tape.head

        let write (c : char) (tape : t) : t =
            { tape with head = c }

        let print (tape : t) : unit =
            List.iter print_char (List.rev tape.left);
            print_char tape.head;
            List.iter print_char tape.right;
            print_newline ();
            print_string (String.make (List.length tape.left) ' ');
            print_char '^';
            print_newline ();
    end

# %%
let primer_trak : unit =
    Tape.(
        make "ABCDE"
        |> move Left
        |> move Left
        |> move Right
        |> move Right
        |> move Right
        |> move Right
        |> write '!'
        |> print
    )

# %%
let primer_trak_dodatno : unit =
    Tape.(
        make "ABC"
        |> move Left
        |> move Left
        |> write '!'
        |> move Right
        |> move Right
        |> print
    )

# %% [markdown]
# ## Implementacija Turingovih strojev

# %% [markdown]
# Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:
#
# - `t` tip Turingovih strojev;
# - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih stanj ter prazno prehodno funkcijo;
# - `initial`, ki vrne začetno stanje stroja;
# - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto (q', a', d)$;
# - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.
#
# Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko predpostavite, da ju bomo klicali le na poprej podanih stanjih.

# Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost kode.

# %%
type instruction = state * char * direction

module StateCharMap : (Map.S with type key = state * char) =
    Map.Make(
        struct
            type t = state * char

            let compare ((st1, c1) : t) ((st2, c2) : t) : int =
                match String.compare st1 st2 with
                | 0 -> Char.compare c1 c2
                | c -> c
        end
    )

# %%
module type MACHINE =
    sig
        type t

        val make : state -> state list -> t
        val initial : t -> state
        val add_transition : state -> char -> state -> char -> direction -> t -> t
        val step : t -> state -> Tape.t -> (state * Tape.t) option
    end

module Machine : MACHINE =
    struct
        type t =
            {
                initial : state;
                transition_map : instruction StateCharMap.t;
            }

        let make (initial : state) (state_list : state list) : t =
            {
                initial = initial;
                transition_map = StateCharMap.empty
            }

        let initial (machine : t) : state =
            machine.initial

        let add_transition (st : state) (c : char) (st' : state) (c' : char) (dir : direction) (machine : t) : t =
            { machine with transition_map = StateCharMap.add (st, c) (st', c', dir) machine.transition_map }

        let step (machine : t) (st : state) (tape : Tape.t) : (state * Tape.t) option =
            let opt_wrapper (f : ('a -> 'b)) (x_opt : 'a option) : 'b option =
                match x_opt with
                | None -> None
                | Some x -> Some (f x)
            and execute_step ((st', c', dir) : instruction) : state * Tape.t =
                (st', Tape.write c' tape |> Tape.move dir)
            in
            StateCharMap.find_opt (st, Tape.read tape) machine.transition_map
            |> opt_wrapper execute_step
    end

# %% [markdown]
# Primer stroja "Binary Increment" na <http://turingmachine.io> lahko implementiramo kot:

# %%
let binary_increment : Machine.t =
    Machine.(
        make "right" [ "carry"; "done" ]
        |> add_transition "right" '1' "right" '1' Right
        |> add_transition "right" '0' "right" '0' Right
        |> add_transition "right" ' ' "carry" ' ' Left
        |> add_transition "carry" '1' "carry" '0' Left
        |> add_transition "carry" '0' "done" '1' Left
        |> add_transition "carry" ' ' "done" '1' Left
    )

# %% [markdown]
# Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz. Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti izvajanja.

# %%
let slow_run (machine : Machine.t) (s : string) : unit =
    let output (st : state) (tape : Tape.t) : unit =
        Tape.print tape;
        print_endline st;
        print_newline ()
    in
    let rec slow_run_aux ((st, tape) : state * Tape.t) : unit =
        match Machine.step machine st tape with
        | None -> output st tape
        | Some (st', tape') ->
            output st tape;
            slow_run_aux (st', tape')
    in
    slow_run_aux (Machine.initial machine, Tape.make s)

let speed_run (machine : Machine.t) (s : string) : unit =
    let rec speed_run_aux ((st, tape) : state * Tape.t) : unit =
        match Machine.step machine st tape with
        | None -> Tape.print tape
        | Some (st', tape') -> speed_run_aux (st', tape')
    in
    speed_run_aux (Machine.initial machine, Tape.make s)

# %%
let primer_slow_run : unit =
    slow_run binary_increment "1011"

# %%
let primer_speed_run : unit =
    speed_run binary_increment "1011"

# %% [markdown]
# ## Krajši zapis

# %% [markdown]
# Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih, nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak in stanje pa pustimo pri miru. Zapišite funkcije:

# - `for_state`
# - `for_character`
# - `for_characters`
# - `move`
# - `switch_and_move`
# - `write_and_move`
# - `write_switch_and_move`
#
# s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj. Implementacijo in tipe ugotovite sami.

# %%
type state_transform = state -> Machine.t -> Machine.t
type char_state_transform = char -> state_transform

let for_state (st : state) (trans_list : state_transform list list) (machine : Machine.t) : Machine.t =
        List.flatten trans_list
        |> List.fold_left
            (
                fun (machine : Machine.t) (trans : state_transform) : Machine.t ->
                trans st machine
            )
            machine

let for_character (c : char) (trans : char_state_transform) : state_transform list =
    [trans c]

let for_characters (s : string) (trans : char_state_transform) : state_transform list =
    let str_to_list (s : string) : char list =
        List.init (String.length s) (String.get s)
    in
    str_to_list s
    |> List.map trans

let move (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st c dir machine

let switch_and_move (st' : state) (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st' c dir machine

let write_and_move (c' : char) (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st c' dir machine

let write_switch_and_move (c' : char) (st' : state) (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st' c' dir machine

# %%
let binary_increment' : Machine.t =
    Machine.make "right" ["carry"; "done"]
    |> for_state "right"
        [
            for_characters "01" @@ move Right;
            for_character ' ' @@ switch_and_move "carry" Left
        ]
    |> for_state "carry"
        [
            for_character '1' @@ write_switch_and_move '0' "carry" Left;
            for_characters "0 " @@ write_switch_and_move '1' "done" Left
        ]

# %%
let primer_krajse_dodatno_1 : unit =
    slow_run binary_increment' "1011"

# %%
let primer_krajse_dodatno_2 : unit =
    speed_run binary_increment' "1011"

# %% [markdown]
# ## Primeri Turingovih strojev

# %% [markdown]
# Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku, kot se je začel prvotni niz.

# %% [markdown]
# ### Obračanje niza
#

# %% [markdown]
# Sestavite Turingov stroj, ki začetni niz obrne na glavo.

# %%
let reverse : Machine.t =
    Machine.make "rename" ["copy"; "write_zero"; "write_one"; "clear"]
    |> for_state "rename"
        [
            for_character '0' @@ write_and_move 'A' Right;
            for_character '1' @@ write_and_move 'B' Right;
            for_character ' ' @@ switch_and_move "copy" Left
        ]
    |> for_state "copy"
        [
            for_characters "_01" @@ move Left;
            for_character 'A' @@ write_switch_and_move '_' "write_zero" Right;
            for_character 'B' @@ write_switch_and_move '_' "write_one" Right;
            for_character ' ' @@ switch_and_move "clear" Right
        ]
    |> for_state "write_zero"
        [
            for_characters "_01" @@ move Right;
            for_character ' ' @@ write_switch_and_move '0' "copy" Left
        ]
    |> for_state "write_one"
        [
            for_characters "_01" @@ move Right;
            for_character ' ' @@ write_switch_and_move '1' "copy" Left
        ]
    |> for_state "clear"
        [
            for_character '_' @@ write_and_move ' ' Right
        ]

# %%
let primer_reverse : unit =
    speed_run reverse "0000111001"

# %%
let primer_reverse_dodatno : unit =
    speed_run reverse ""

# %% [markdown]
# ### Podvajanje niza

# %% [markdown]
# Sestavite Turingov stroj, ki podvoji začetni niz.

# %%
let duplicate : Machine.t =
    Machine.make "rename" ["mark"; "copy"; "write_zero"; "write_one"; "return"]
    |> for_state "rename"
        [
            for_character '0' @@ write_and_move 'A' Right;
            for_character '1' @@ write_and_move 'B' Right;
            for_character ' ' @@ switch_and_move "copy" Left
        ]
    |> for_state "copy"
        [
            for_character 'A' @@ write_switch_and_move 'X' "write_zero" Left;
            for_character 'B' @@ write_switch_and_move 'Y' "write_one" Left;
            for_characters "01" @@ move Left;
            for_character ' ' @@ switch_and_move "end" Right
        ]
    |> for_state "write_zero"
        [
            for_characters "01AB" @@ move Left;
            for_character ' ' @@ write_switch_and_move '0' "return" Right;
        ]
    |> for_state "write_one"
        [
            for_characters "01AB" @@ move Left;
            for_character ' ' @@ write_switch_and_move '1' "return" Right;
        ]
    |> for_state "return"
        [
            for_characters "01AB" @@ move Right;
            for_character 'X' @@ write_switch_and_move '0' "copy" Left;
            for_character 'Y' @@ write_switch_and_move '1' "copy" Left
        ]

# %%
let primer_duplicate =
    speed_run duplicate "010011"

# %% [markdown]
# ### Eniški zapis
#

# %% [markdown]
# Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.

# %%
let to_unary : Machine.t =
    Machine.make "rename" ["subtract"; "add"; "clear"]
    |> for_state "rename"
        [
            for_character '0' @@ write_and_move 'A' Right;
            for_character '1' @@ write_and_move 'B' Right;
            for_character ' ' @@ switch_and_move "subtract" Left
        ]
    |> for_state "subtract"
        [
            for_character 'A' @@ write_and_move 'B' Left;
            for_character 'B' @@ write_switch_and_move 'A' "add" Right;
            for_character '1' @@ move Left;
            for_character ' ' @@ switch_and_move "clear" Right
        ]
    |> for_state "add"
        [
            for_characters "1AB" @@ move Right;
            for_character ' ' @@ write_switch_and_move '1' "subtract" Left
        ]
    |> for_state "clear"
        [
            for_character 'B' @@ write_and_move ' ' Right
        ]

# %%
let primer_to_unary : unit =
    speed_run to_unary "1010"

# %%
let primer_to_unary_dodatno : unit =
    speed_run to_unary "0"

# %% [markdown]
# ### Dvojiški zapis
#

# %% [markdown]
# Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v dvojiškem zapisu.

# %%
let to_binary : Machine.t =
    Machine.make "rename" ["return"; "subtract"; "add"; "clear"]
    |> for_state "rename"
        [
            for_character '1' @@ write_and_move 'A' Right;
            for_character ' ' @@ switch_and_move "return" Left
        ]
    |> for_state "return"
        [
            for_character 'A' @@ move Left;
            for_character ' ' @@ switch_and_move "subtract" Right;
        ]
    |> for_state "subtract"
        [
            for_character 'A' @@ write_switch_and_move '_' "add" Left;
            for_characters "_01" @@ move Right;
            for_character ' ' @@ switch_and_move "clear" Left
        ]
    |> for_state "add"
        [
            for_character '_' @@ move Left;
            for_characters " 0" @@ write_switch_and_move '1' "subtract" Right;
            for_character '1' @@ write_and_move '0' Left
        ]
    |> for_state "clear"
        [
            for_character '_' @@ write_and_move ' ' Left;
            for_characters "01" @@ move Left;
            for_character ' ' @@ switch_and_move "end" Right;
        ]

# %%
let primer_to_binary : unit =
    speed_run to_binary (String.make 42 '1')

# %%
let primer_to_binary_dodatno : unit =
    speed_run to_binary "1"
