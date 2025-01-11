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

        let empty_tape : t = { left = []; head = ' '; right = [] }

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
            | [] -> order ' ' ([], tape.head :: other_chars)
            | hd' :: dir_chars' -> order hd' (dir_chars', trim_others other_chars)

        let read (tape : t) : char = tape.head

        let write (c : char) (tape : t) : t = { tape with head = c }

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
(*module OrderedState : Map.OrderedType =*)
(*    struct*)
(*        type t = state*)
(*        let compare (s1 : state) (s2 : state) : int = String.compare s1 s2*)
(*    end*)
(**)
(*module OrderedChar : Map.OrderedType =*)
(*    struct*)
(*        type t = char*)
(*        let compare (c1 : char) (c2 : char) : int = Char.compare c1 c2*)
(*    end*)

module StateMap : (Map.S with type key = state) = Map.Make(String)
module CharMap : (Map.S with type key = char) = Map.Make(Char)

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
                transition : ((state * char * direction) CharMap.t) StateMap.t;
            }

        let make (initial : state) (state_list : state list) : t =
            let map_add (map : ('a CharMap.t) StateMap.t) (key : state) : ('a CharMap.t) StateMap.t =
                StateMap.add key (CharMap.empty) map
            in
            {
                initial = initial;
                transition = List.fold_left map_add StateMap.empty (initial :: state_list)
            }

        let initial (machine : t) : state = machine.initial

        let add_transition (st : state) (c : char) (st' : state) (c' : char) (dir : direction) (machine : t) : t =
            let char_map_add (char_map_opt : 'a CharMap.t option) : 'a CharMap.t option =
                match char_map_opt with
                | None -> None
                | Some char_map -> Some (CharMap.add c (st', c', dir) char_map)
            in
            { machine with transition = StateMap.update st char_map_add machine.transition }

        let step (machine : t) (st : state) (tape : Tape.t) : (state * Tape.t) option =
            let opt_wrapper : type a b. (a -> b option) -> a option -> b option =
                fun (f : a -> b option) (x_opt : a option) : b option ->
                    match x_opt with
                    | None -> None
                    | Some x -> f x
            and execute_step ((st', c', dir) : state * char * direction) : (state * Tape.t) option =
                Some (st', Tape.write c' tape |> Tape.move dir)
            in
            StateMap.find_opt st machine.transition
            |> opt_wrapper (CharMap.find_opt (Tape.read tape))
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
let run (f : state -> Tape.t -> unit) (machine : Machine.t) (s : string) : unit =
    let rec run_aux (instructions_opt : (state * Tape.t) option) : unit =
        match instructions_opt with
        | None -> ()
        | Some (st, tape) ->
            f st tape;
            run_aux (Machine.step machine st tape)
    in
    Some (Machine.initial machine, Tape.make s)
    |> run_aux

let slow_run (machine : Machine.t) (s : string) : unit =
    let output (st : state) (tape : Tape.t) : unit =
        Tape.print tape;
        print_endline st;
        print_newline ()
    in
    run output machine s

let speed_run (machine : Machine.t) (s : string) : unit =
    run (fun _ _ : unit -> ()) machine s

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
#
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
let binary_increment' =
    Machine.make "right" ["carry"; "done"]
    |> for_state "right" [
        for_characters "01" @@ move Right;
        for_character ' ' @@ switch_and_move "carry" Left
    ]
    |> for_state "carry" [
        for_character '1' @@ switch_and_move "carry" Left;
        for_characters "0 " @@ write_switch_and_move '1' "done" Left
    ]

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
let primer_reverse = speed_run reverse "0000111001"

# %% [markdown]
# ### Podvajanje niza
#

# %% [markdown]
# Sestavite Turingov stroj, ki podvoji začetni niz.

# %%
let primer_duplicate = speed_run duplicate "010011"

# %% [markdown]
# ### Eniški zapis
#

# %% [markdown]
# Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.

# %%
let primer_to_unary = speed_run to_unary "1010"

# %% [markdown]
# ### Dvojiški zapis
#

# %% [markdown]
# Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v dvojiškem zapisu.

# %%
let primer_to_binary = speed_run to_binary (String.make 42 '1')

