# %% [markdown]
# # 1. domača naloga

# %% [markdown]
# ## Ogrevanje

# %% [markdown]
# ### Števke

# %% [markdown]
# Napišite funkcijo `stevke : int -> int -> int list`, ki sprejme pozitivni celi števili $b$ in $n$ ter vrne seznam števk števila $n$ v bazi $b$. Pri tem tudi za baze, ki so večje od $10$, uporabimo števke od $0$ do $b - 1$.

# %%
let stevke (b : int) (n : int) : int list =
    let rec stevke_aux (acc : int list) (n : int) : int list =
        match n with
        | 0 -> acc
        | n -> stevke_aux ((n mod b) :: acc) (n / b)
    in
    stevke_aux [] n

# %%
stevke 10 12345

# %%
stevke 2 42

# %%
stevke 16 (3 * 16 * 16 * 16 + 14 * 16 * 16 + 15 * 16 + 9)

# %% [markdown]
# ### Začetek seznama

# %% [markdown]
# Napišite funkcijo `take : int -> 'a list -> 'a list`, ki sprejme naravno število in vrne ustrezno število elementov z začetka danega seznama. Če je podani seznam krajši od zahtevane dolžine, naj funkcija vrne kar celoten seznam.

# %%
let take (n : int) (list: 'a list) : 'a list =
    let rec take_aux (acc : 'a list) (n : int) (list : 'a list) : 'a list =
        match list with
        | _ when n <= 0 -> acc
        | [] -> acc
        | head :: tail -> take_aux (head :: acc) (n - 1) tail
    in
    List.rev (take_aux [] n list)

# %%
take 3 [1; 2; 3; 4; 5]

# %%
take 10 [1; 2; 3; 4; 5]

# %% [markdown]
# ### Odstranjevanje ujemajočih

# %% [markdown]
# Napišite funkcijo `drop_while : ('a -> bool) -> 'a list -> 'a list`, ki z začetka seznama odstrani vse elemente, ki zadoščajo danemu predikatu. Ko najde element, ki predikatu ne zadošča, vrne preostanek seznama.

# %%
let rec drop_while (p : 'a -> bool) (list : 'a list) : 'a list =
    match list with
    | [] -> []
    | head :: tail when not (p head) -> head :: tail
    | head :: tail -> drop_while p tail

# %%
drop_while (fun x -> x < 5) [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]

# %%
drop_while (fun x -> x < 5) [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]

# %% [markdown]
# ### Funkcija `filter_mapi`

# %% [markdown]
# Napišite funkcijo `filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list`, ki deluje tako kot `List.filter_map`, le da funkcija poleg elemenov dobi še njihove indekse.

# %%
let filter_mapi (f : int -> 'a -> 'b option) (list : 'a list) : 'b list =
    let rec filter_mapi_aux (acc : 'b list) (i : int) (list : 'a list) : 'b list =
        match list with
        | [] -> acc
        | head :: tail -> filter_mapi_aux
            (
                match f i head with
                | None -> acc
                | Some x -> x :: acc
            )
            (i + 1) tail
    in
    filter_mapi_aux [] 0 list
    |> List.rev

# %%
filter_mapi
  (fun i x -> if i mod 2 = 0 then Some (x * x) else None)
  [1; 2; 3; 4; 5; 6; 7; 8; 9]

# %% [markdown]
# ## Curry-Howardov izomorfizem

# %% [markdown]
# Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt predstavimo s produktnim, eksponent pa s funkcijskim tipom.
#
# Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
#   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b \in B \}$$
# predstavimo s tipom `('a, 'b) sum`, definiranim z:

# %%
type ('a, 'b) sum = In1 of 'a | In2 of 'b

# %% [markdown]
# Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo spodnje izomorfizme množic.
#

# %% [markdown]
# ### $A \times B \cong B \times A$

# %%
let phi1 ((a, b) : 'a * 'b) : 'b * 'a = (b, a)

let psi1 ((b, a) : 'b * 'a) : 'a * 'b = (a, b)

# %% [markdown]
Še lažje pa lahko nardimo kar:

# %%
let psi1 = phi1

# %% [markdown]
# ### $A + B \cong B + A$

# %%
let phi2 (x : ('a, 'b) sum) : ('b, 'a) sum =
    match x with
    | In1 a -> In2 a
    | In2 b -> In1 b

let psi2 (x : ('b, 'a) sum) : ('a, 'b) sum =
    match x with
    | In1 b -> In2 b
    | In2 a -> In1 a

# %% [markdown]
# ### $A \times (B \times C) \cong (A \times B) \times C$

# %%
let phi3 ((a, (b, c)) : 'a * ('b * 'c)) : ('a * 'b) * 'c = ((a, b), c)

let psi3 (((a, b), c) : ('a * 'b) * 'c) : 'a * ('b * 'c) = (a, (b, c))


# %% [markdown]
# ### $A + (B + C) \cong (A + B) + C$

# %%
let phi4 (x : ('a, ('b, 'c) sum) sum) : (('a, 'b) sum, 'c) sum =
    match x with
    | In1 a -> In1 (In1 a)
    | In2 (In1 b) -> In1 (In2 b)
    | In2 (In2 c) -> In2 c

let psi4 (x : (('a, 'b) sum, 'c) sum) : ('a, ('b, 'c) sum) sum =
    match x with
    | In1 (In1 a) -> In1 a
    | In1 (In2 b) -> In2 (In1 b)
    | In2 c -> In2 (In2 c)

# %% [markdown]
# ### $A \times (B + C) \cong (A \times B) + (A \times C)$

# %%
let phi5 ((a, x) : 'a * ('b, 'c) sum) : ('a * 'b, 'a * 'c) sum =
    match x with
    | In1 b -> In1 (a, b)
    | In2 c -> In2 (a, c)

let psi5 (x : ('a * 'b, 'a * 'c) sum) : 'a * ('b, 'c) sum =
    match x with
    | In1 (a, b) -> (a, In1 b)
    | In2 (a, c) -> (a, In2 c)

# %% [markdown]
# ### $A^{B + C} \cong A^B \times A^C$

# %%
let phi6 (f : ('b, 'c) sum -> 'a) : ('b -> 'a) * ('c -> 'a) =
    (
        (fun (b : 'b) : 'a -> f (In1 b)),
        (fun (c : 'c) : 'a -> f (In2 c))
    )

let psi6 ((f, g) : ('b -> 'a) * ('c -> 'a)) : ('b, 'c) sum -> 'a =
    fun (x : ('b, 'c) sum) : 'a ->
        match x with
        | In1 b -> f b
        | In2 c -> g c

# %% [markdown]
# ### $(A \times B)^C \cong A^C \times B^C$

# %%
let phi7 (f : 'c -> 'a * 'b) : ('c -> 'a) * ('c -> 'b) =
    (
        (fun (c : 'c) : 'a -> fst (f c)),
        (fun (c : 'c) : 'b -> snd (f c))
    )

let psi7 ((f, g) : ('c -> 'a) * ('c -> 'b)) : 'c -> 'a * 'b =
    fun (c : 'c) : ('a * 'b) -> (f c, g c)

# %% [markdown]
# ## Polinomi

# %% [markdown]
# Polinome $a_0 + a_1 x + \cdots + a_n x^n$ predstavimo s seznami celoštevilskih koeficientov od prostega do vodilnega člena. Na primer, polinom $1 - 2 x + 3 x^2$ predstavimo s seznamom `[1; -2; 3]`.

# %%
type polinom = int list

# %% [markdown]
# ### Odstranjevanje odvečnih ničel

# %% [markdown]
# Napišite funkcijo `pocisti : polinom -> polinom`, ki s konca seznama koeficientov odstrani odvečne ničle.

# %%
let pocisti (p : polinom) : polinom =
    let rec pocisti_aux (p : polinom) : polinom =
        match p with
        | [] -> []
        | 0 :: tail -> pocisti_aux tail
        | _ -> p
    in
    List.rev p
    |> pocisti_aux
    |> List.rev

# %%
pocisti [1; -2; 3; 0; 0]

# %% [markdown]
# ### Seštevanje

# %% [markdown]
# Napišite funkcijo `( +++ ) : polinom -> polinom -> polinom`, ki sešteje dva polinoma.

# %%
(*let rec ( +++ ) (p1 : polinom) (p2 : polinom) : polinom =*)
(*    match p1, p2 with*)
(*    | ([], _ | _, []) -> p1 @ p2*)
(*    | head1 :: tail1, head2 :: tail2 ->*)
(*        (head1 + head2) :: (tail1 +++ tail2)*)
(*        |> pocisti*)

let ( +++ ) (p1 : polinom) (p2 : polinom) : polinom =
    let rec sum_aux (acc : polinom) (p1 : polinom) (p2 : polinom) : polinom =
        match p1, p2 with
        | ([], _ | _, []) -> pocisti (List.rev acc @ p1 @ p2)
        | head1 :: tail1, head2 :: tail2 -> sum_aux ((head1 + head2) :: acc) tail1 tail2
    in
    sum_aux [] p1 p2

# %%
[1; -2; 3] +++ [1; 2]

# %%
[1; -2; 3] +++ [1; 2; -3]

# %% [markdown]
# ### Množenje

# %% [markdown]
# Napišite funkcijo `( *** ) : polinom -> polinom -> polinom`, ki zmnoži dva polinoma.

# %%
let rec ( *** ) (p1 : polinom) (p2 : polinom) : polinom =
    let rec mul_aux (acc : polinom) (i : int) (p1 : polinom) (p2 : polinom) : polinom =
        match p1 with
        | [] -> []
        | head :: [] -> mul_aux
        | head :: tail -> mul_aux 0 p2 []

# %%
[1; 1] *** [1; 1] *** [1; 1]

# %%
[1; 1] *** [1; -1]

# %% [markdown]
# ### Izračun vrednosti v točki

# %% [markdown]
# Napišite funkcijo `vrednost : polinom -> int -> int`, ki izračuna vrednost polinoma v danem argumentu.

# %%
let vrednost (p : polinom) (x : int) : int =
    let rec vrednost_aux (acc : int) (p : polinom) =
        match p with
        | [] -> acc
        | head :: tail -> vrednost_aux (head + acc * x) tail
    in
    vrednost_aux 0 (List.rev p)

# %%
vrednost [1; -2; 3] 2

# %% [markdown]
# ### Odvajanje

# %% [markdown]
# Napišite funkcijo `odvod : polinom -> polinom`, ki izračuna odvod polinoma.

# %%
let odvod (p : polinom) : polinom =
    let rec odvod_aux (acc : polinom) (i : int) (p : polinom) : polinom =
        match p with
        | ([] | _ :: []) -> acc
        | _ :: head2 :: tail -> odvod_aux ((i * head2) :: acc) (i + 1) (head2 :: tail)
    in
    odvod_aux [] 1 p
    |> List.rev

# %%
odvod [1; -2; 3]

# %% [markdown]
# ### Lep izpis

# %% [markdown]
# Napišite funkcijo `izpis : polinom -> string`, ki polinom lepo izpiše. Na primer, `izpis [1; -2; 3]` vrne `"3 x^2 - 2 x + 1"` oziroma še bolje kot `"3 x² - 2 x + 1"`. Pozorni bodite, da izpis začnete z vodilnim členom.

# %%
let izpis (p : polinom) : string =
    let rec get_digits (acc : int list) (n : int) : int list =
        match n with
        | n when n < 0 -> get_digits acc (-n)
        | n when n < 10 -> n :: acc
        | n -> get_digits ((n mod 10) :: acc) (n / 10)
    and unicode_superscript_of_digit (d : int) : string =
        let superscript_list : string list = ["⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"]
        in
        match d with
        | n when 0 <= n && n < 10 -> List.nth superscript_list n
        | _ -> ""
    in
    let rec unicode_superscript (acc : string) (digits : int list) : string =
        match digits with
        | [] -> acc
        | head :: tail -> unicode_superscript (acc ^ unicode_superscript_of_digit head) tail
    in
    let factor_const (is_leading : bool) (is_constant : bool) (n : int) : string =
        let plus_sign : string = if is_leading then "" else " + "
        and minus_sign : string = if is_leading then "- " else " - "
        and padding : string = if is_constant then "" else " "
        in
        let one_sign : string = if is_constant then "1" ^ padding else ""
        in
        match n with
        | 1 -> plus_sign ^ one_sign
        | -1 -> minus_sign ^ one_sign
        | n when n >= 0 -> plus_sign ^ string_of_int n ^ padding
        | n -> minus_sign ^ string_of_int (-n) ^ padding
    and factor_power (i : int) : string =
        match i with
        | 0 -> ""
        | 1 -> "x"
        | i when i > 1 -> "x" ^ unicode_superscript "" (get_digits [] i)
        | i -> "x^(" ^ string_of_int i ^ ")"
    in
    let rec izpis_aux (acc : string) (i : int) (p : polinom) =
        match p with
        | [] -> acc
        | head :: tail -> izpis_aux
            (
                match head with
                | 0 -> acc
                | n -> (factor_const (tail = []) (i = 0) n) ^ (factor_power i) ^ acc
            )
            (i + 1) tail
    in
    izpis_aux "" 0 p

# %%
izpis [1; 2; 1]

# %%
izpis [1; 0; -1; 0; 1; 0; -1; 0; 1; 0; -1; 0; 1]

# %%
izpis [0; -3; 3; -1]

# %% [markdown]
# ## Samodejno odvajanje

# %% [markdown]
# Ob razmahu strojnega učenja, ki optimalno rešitev išče s pomočjo gradientnega spusta, si želimo čim bolj enostavno računati odvode. Odvod funkcije $f$ v točki $x_0$ lahko seveda ocenimo tako, da v
#
# $$\frac{f (x_0 + h) - f(x_0)}{h}$$
#
# vstavimo dovolj majhno število $h$.

# %%
let priblizek_odvoda (f : float -> float) (x0 : float) (h : float) : float = (f (x0 +. h) -. f x0) /. h

# %%
let f (x : float) : float = sin x +. cos x +. exp x in
List.map (priblizek_odvoda f 1.) [0.1; 0.01; 0.001; 0.0001; 0.00001]

# %% [markdown]
# Pri samodejnem odvajanju izkoristimo to, da poznamo odvode elementarnih funkcij, odvode sestavljenih funkcij pa lahko izračunamo iz posameznih odvodov. Tako bomo vsako funkcijo predstavili s parom: prvotno funkcijo in njenim odvodom.

# %%
type odvedljiva = (float -> float) * (float -> float)

# %%
let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, (fun x -> -. sin x))
let eksp : odvedljiva = (exp, exp)
let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
    fun (f, f') (g, g') -> ((fun x -> f x +. g x), (fun x -> f' x +. g' x))

# %%
let (_, f') : odvedljiva = sinus ++. kosinus ++. eksp in
f' 1.

# %% [markdown]
# ### Vrednost odvoda

# %% [markdown]
# Napišite funkciji `vrednost : odvedljiva -> float -> float` in `odvod : odvedljiva -> float -> float`, ki izračunata vrednost funkcije in njenega odvoda v danem argumentu.

# %%
let vrednost ((f, _) : odvedljiva) (x0 : float) : float = f x0
let odvod ((_, f') : odvedljiva) (x0 : float) : float = f' x0

# %% [markdown]
# ### Osnovne funkcije

# %% [markdown]
# Napišite funkciji `konstanta : float -> odvedljiva` in `identiteta : odvedljiva`, ki predstavljata konstantno in identično funkcijo.

# %%
let konstanta (c : float) : odvedljiva =
    (
        (fun (x : float) : float -> c),
        (fun (x : float) : float -> 0.)
    )

let identiteta : odvedljiva =
    (
        (fun (x : float) : float -> x),
        (fun (x : float) : float -> 1.)
    )

# %% [markdown]
# ### Produkt in kvocient

# %% [markdown]
# Napišite funkciji `( **. ) : odvedljiva -> odvedljiva -> odvedljiva` in `( //. ) : odvedljiva -> odvedljiva -> odvedljiva`, ki predstavljata produkt in kvocient dveh odvedljivih funkcij.

# %%
let ( **. ) ((f, f') : odvedljiva) ((g, g') : odvedljiva) : odvedljiva =
    (
        (fun (x : float) : float -> f x *. g x),
        (fun (x : float) : float -> f' x *. g x +. f x *. g' x)
    )

let ( //. ) ((f, f') : odvedljiva) ((g, g') : odvedljiva) : odvedljiva =
    (
        (fun (x : float) : float -> f x /. g x),
        (fun (x : float) : float -> (f' x *. g x -. f x *. g' x) /. (g x *. g x))
    )

# %%
let kvadrat = identiteta **. identiteta

# %% [markdown]
# ### Kompozitum

# %% [markdown]
# Napišite funkcijo `( @@. ) : odvedljiva -> odvedljiva -> odvedljiva`, ki predstavlja kompozitum dveh odvedljivih funkcij.

# %%
let ( @@. ) ((f, f') : odvedljiva) ((g, g') : odvedljiva) : odvedljiva =
    (
        (fun (x : float) : float -> f (g x)),
        (fun (x : float) : float -> f' (g x) *. g' x)
    )


# %%
let vedno_ena : odvedljiva = (kvadrat @@. sinus) ++. (kvadrat @@. kosinus)

# %%
vrednost vedno_ena 12345.

# %%
odvod vedno_ena 12345.

# %% [markdown]
# ## Substitucijska šifra

# %% [markdown]
# Substitucijska šifra je preprosta šifra, pri kateri črke abecede med seboj permutiramo. Na primer, če bi (angleško) abecedo permutirali kot
#
# ```
# A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
# T H E Q U I C K B R W N F X J M P S O V L A Z Y D G
# ```
#
# bi besedo `HELLO` šifrirali kot `KUNNJ`. Ključe, s katerimi šifriramo besedila bomo predstavili kar z nizi črk, v katere se slikajo črke abecede.

# %%
let quick_brown_fox : string = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM"

# %% [markdown]
# Včasih bomo v primerih uporabljali tudi krajše ključe, a vedno lahko predpostavite, da bodo ključi permutacije začetnega dela abecede. Prav tako si pri delu lahko pomagate s funkcijama `indeks` in `crka`:

# %%
let indeks (c :char) : int = Char.code c - Char.code 'A'
let crka (i : int) : char = Char.chr (i + Char.code 'A')

# %% [markdown]
# Predno začnemo reševati pa lahko napišemo še eno uporabno funkcijo `explode : string -> char list`, ki nam niz pretvori v seznam znakov. -Andrej Anžlovar

# %%
let explode (string : string) : char list =
    let rec explode_aux (acc : char list) (i : int) : char list =
        match i with
        | i when i < 0 -> acc
        | i -> explode_aux (string.[i] :: acc) (i - 1)
    in
    explode_aux [] (String.length string - 1)

# %% [markdown]
# ### Šifriranje

# %% [markdown]
# Napišite funkcijo `sifriraj : string -> string -> string`, ki besedilo šifrira z danim ključem. Vse znake, ki niso velike tiskane črke, pustimo pri miru.

# %%
let sifriraj (key : string) (string : string) : string =
    let rec sifriraj_aux (acc : string) (chars : char list) : string =
        match chars with
        | [] -> acc
        | head :: tail -> sifriraj_aux
            (
                let i : int = indeks head
                in
                acc ^ String.make 1
                (
                    if 0 <= i && i < (String.length key) then key.[i]
                    else head
                )
            )
            tail
    in
    sifriraj_aux "" (explode string)

# %%
sifriraj quick_brown_fox "HELLO, WORLD!"

# %%
"VENI, VIDI, VICI" |> sifriraj rot13

# %%
"VENI, VIDI, VICI" |> sifriraj rot13 |> sifriraj rot13

# %% [markdown]
# ### Inverzni ključ

# %% [markdown]
# Napišite funkcijo `inverz : string -> string`, ki iz ključa izračuna njegov inverz.

# %%
let inverz (key : string) : string =
    let rec get_index (i : int) (c : char) (chars : char list) : int option =
        match chars with
        | [] -> None
        | head :: _ when head = c -> Some i
        | _ :: tail -> get_index (i + 1) c tail
    and key_chars = explode key
    in
    let rec inverz_aux (acc : string) (c : char) : string =
        let ci : int = indeks c
        in
        if String.length key <= ci then acc
        else inverz_aux
            (
                match get_index 0 c key_chars with
                | None -> acc ^ "_"
                | Some i -> acc ^ String.make 1 (crka i)
            )
            (Char.chr (Char.code c + 1))
    in
    inverz_aux "" 'A'

# %%
inverz quick_brown_fox

# %%
inverz rot13 = rot13

# %%
inverz "BCDEA"

# %%
inverz "ABCDE________________H_"

# %% [markdown]
# ### Ugibanje ključa

# %% [markdown]
# Včasih seveda ne poznamo ključa, a vemo, da je besedilo v angleščini. Tako lahko ključ poskusimo uganiti tako, da šifrirane besede paroma primerjamo z besedami iz slovarja, ki smo si ga sposodili [s spleta](https://gist.github.com/deekayen/4148741).

# %%
let besede : string = "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see him two has look more day could go come did number sound no most people my over know water than call first who may down side been now find any new work part take get place made live where after back little only round man year came show every good me give our under name very through just form sentence great think say help low line differ turn cause much mean before move right boy old too same tell does set three want air well also play small end put home read hand port large spell add even land here must big high such follow act why ask men change went light kind off need house picture try us again animal point mother world near build self earth father head stand own page should country found answer school grow study still learn plant cover food sun four between state keep eye never last let thought city tree cross farm hard start might story saw far sea draw left late run don't while press close night real life few north open seem together next white children begin got walk example ease paper group always music those both mark often letter until mile river car feet care second book carry took science eat room friend began idea fish mountain stop once base hear horse cut sure watch color face wood main enough plain girl usual young ready above ever red list though feel talk bird soon body dog family direct pose leave song measure door product black short numeral class wind question happen complete ship area half rock order fire south problem piece told knew pass since top whole king space heard best hour better true . during hundred five remember step early hold west ground interest reach fast verb sing listen six table travel less morning ten simple several vowel toward war lay against pattern slow center love person money serve appear road map rain rule govern pull cold notice voice unit power town fine certain fly fall lead cry dark machine note wait plan figure star box noun field rest correct able pound done beauty drive stood contain front teach week final gave green oh quick develop ocean warm free minute strong special mind behind clear tail produce fact street inch multiply nothing course stay wheel full force blue object decide surface deep moon island foot system busy test record boat common gold possible plane stead dry wonder laugh thousand ago ran check game shape equate hot miss brought heat snow tire bring yes distant fill east paint language among grand ball yet wave drop heart am present heavy dance engine position arm wide sail material size vary settle speak weight general ice matter circle pair include divide syllable felt perhaps pick sudden count square reason length represent art subject region energy hunt probable bed brother egg ride cell believe fraction forest sit race window store summer train sleep prove lone leg exercise wall catch mount wish sky board joy winter sat written wild instrument kept glass grass cow job edge sign visit past soft fun bright gas weather month million bear finish happy hope flower clothe strange gone jump baby eight village meet root buy raise solve metal whether push seven paragraph third shall held hair describe cook floor either result burn hill safe cat century consider type law bit coast copy phrase silent tall sand soil roll temperature finger industry value fight lie beat excite natural view sense ear else quite broke case middle kill son lake moment scale loud spring observe child straight consonant nation dictionary milk speed method organ pay age section dress cloud surprise quiet stone tiny climb cool design poor lot experiment bottom key iron single stick flat twenty skin smile crease hole trade melody trip office receive row mouth exact symbol die least trouble shout except wrote seed tone join suggest clean break lady yard rise bad blow oil blood touch grew cent mix team wire cost lost brown wear garden equal sent choose fell fit flow fair bank collect save control decimal gentle woman captain practice separate difficult doctor please protect noon whose locate ring character insect caught period indicate radio spoke atom human history effect electric expect crop modern element hit student corner party supply bone rail imagine provide agree thus capital won't chair danger fruit rich thick soldier process operate guess necessary sharp wing create neighbor wash bat rather crowd corn compare poem string bell depend meat rub tube famous dollar stream fear sight thin triangle planet hurry chief colony clock mine tie enter major fresh search send yellow gun allow print dead spot desert suit current lift rose continue block chart hat sell success company subtract event particular deal swim term opposite wife shoe shoulder spread arrange camp invent cotton born determine quart nine truck noise level chance gather shop stretch throw shine property column molecule select wrong gray repeat require broad prepare salt nose plural anger claim continent oxygen sugar death pretty skill women season solution magnet silver thank branch match suffix especially fig afraid huge sister steel discuss forward similar guide experience score apple bought led pitch coat mass card band rope slip win dream evening condition feed tool total basic smell valley nor double seat arrive master track parent shore division sheet substance favor connect post spend chord fat glad original share station dad bread charge proper bar offer segment slave duck instant market degree populate chick dear enemy reply drink occur support speech nature range steam motion path liquid log meant quotient teeth shell neck"

# %% [markdown]
# Sestavite vrednost `slovar : string list`, ki vsebuje vse besede iz slovarja, pretvorjene v velike tiskane črke.

# %%
let slovar : string list = String.split_on_char ' ' (String.uppercase_ascii besede)

# %%
take 42 slovar

# %%
List.nth slovar 321

# %% [markdown]
# ### Razširjanje ključa s črko

# %% [markdown]
# Med ugibanjem seveda ne bomo poznali celotnega ključa. V tem primeru bomo za neznane črke uporabili znak `_`. Na primer, če bi vedeli, da je črka `A` v besedilu šifrirana kot `X`, črka `C` pa kot `Y`, bi ključ zapisali kot `"X_Y_______________________"`.

# Napišite funkcijo `dodaj_zamenjavo : string -> char * char -> string option`, ki sprejme ključ ter ga poskusi razširiti z zamenjavo dane črke. Funkcija naj vrne `None`, če razširitev vodi v ključ, ki ni bijektiven (torej če ima črka že dodeljeno drugo zamenjavo ali če smo isto zamenjavo dodelili dvema različnima črkama).

# %%
let dodaj_zamenjavo (partial_key : string) ((c1, c2) : char * char) : string option =
    let rec contains (c : char) (chars : char list) : bool =
        match chars with
        | [] -> false
        | head :: _ when head = c -> true
        | _ :: tail -> contains c tail
    and replace (acc : string) (i : int) (c : char) (chars : char list) : string =
        match chars with
        | [] -> acc
        | head :: tail -> replace (acc ^ String.make 1 (if i = 0 then c else head)) (i - 1) c tail
    and c1i : int = indeks c1
    and key_chars : char list = explode partial_key
    in
    if c1i < 0 || String.length partial_key <= c1i then None
    else
        match partial_key.[c1i] with
        | x when x = c2 -> Some partial_key
        | '_' -> if contains c2 key_chars then None else Some (replace "" c1i c2 key_chars)
        | _ -> None

# %%
dodaj_zamenjavo "AB__E" ('C', 'X')

# %%
dodaj_zamenjavo "ABX_E" ('C', 'X')

# %%
dodaj_zamenjavo "ABY_E" ('C', 'E')

# %% [markdown]
# ### Razširjanje ključa z besedo

# %% [markdown]
# S pomočjo funkcije `dodaj_zamenjavo` sestavite še funkcijo `dodaj_zamenjave : string -> string * string -> string option`, ki ključ razširi z zamenjavami, ki prvo besedo preslikajo v drugo.

# %%
let dodaj_zamenjave (partial_key : string) ((string, encoded) : string * string) : string option =
    let rec dodaj_zamenjave_aux (acc : string option) ((chars, encoded_chars) : char list * char list) : string option =
        match chars, encoded_chars with
        | [], [] -> acc
        | ([], _ | _, []) -> None
        | head1 :: tail1, head2 :: tail2 -> dodaj_zamenjave_aux
            (
                match acc with
                | None -> None
                | Some key -> dodaj_zamenjavo key (head1, head2)
            )
            (tail1, tail2)
    in
    if String.length string <> String.length encoded then None
    else dodaj_zamenjave_aux (Some partial_key) ((explode string), (explode encoded))

# %%
dodaj_zamenjave "__________________________" ("HELLO", "KUNNJ")

# %%
dodaj_zamenjave "ABCDU_____________________" ("HELLO", "KUNNJ")

# %%
dodaj_zamenjave "ABCDE_____________________" ("HELLO", "KUNNJ")

# %% [markdown]
# ### Vse možne razširitve

# %% [markdown]
# Sestavite funkcijo `mozne_razsiritve : string -> string -> string list -> string list`, ki vzame ključ, šifrirano besedo ter slovar vseh možnih besed, vrne pa seznam vseh možnih razširitev ključa, ki šifrirano besedo slikajo v eno od besed v slovarju.

# %%
let mozne_razsiritve (partial_key : string) (encoded : string) (dictionary : string list) : string list =
    let rec mozne_razsiritve_aux (acc : string list) (dictionary : string list) : string list =
        match dictionary with
        | [] -> acc
        | head :: tail -> mozne_razsiritve_aux
            (
                match dodaj_zamenjave partial_key (head, encoded) with
                | None -> acc
                | Some key -> key :: acc
            )
            tail
    in
    mozne_razsiritve_aux [] dictionary
    |> List.rev

# %%
slovar
|> mozne_razsiritve (String.make 26 '_') "KUNNJ"
|> List.map (fun kljuc -> (kljuc, sifriraj (inverz kljuc) "KUNNJ"))

# %% [markdown]
# ### Odšifriranje

# %% [markdown]
# Napišite funkcijo `odsifriraj : string -> string option`, ki sprejme šifrirano besedilo in s pomočjo slovarja besed ugane odšifrirano besedilo. Funkcija naj vrne `None`, če ni mogoče najti nobenega ustreznega ključa.

# %%
let odsifriraj (string : string) : string list option =
    let rec odsifriraj_aux (encoded_words : string list) (partial_keys : string list) : string list =
        match encoded_words with
        | [] -> partial_keys
        | head :: tail -> odsifriraj_aux tail
            (
                let mozne_razsiritve_rev (encoded : string) (dictionary : string list) (partial_key : string) : string list =
                    mozne_razsiritve partial_key encoded dictionary
                in
                List.concat_map (mozne_razsiritve_rev head slovar) partial_keys
            )
    in
    let keys : string list = odsifriraj_aux (String.split_on_char ' ' string) [String.make 26 '_']
    in
    match keys with
    | [] -> None
    | _ -> Some (List.map (fun (key : string) : string -> sifriraj (inverz key) string) keys)

# %%
sifriraj quick_brown_fox "THIS IS A VERY HARD PROBLEM"

# %%
odsifriraj "VKBO BO T AUSD KTSQ MSJHNUF"
