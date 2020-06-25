(* 
要素数が n の配列 a をシャッフルする(添字は0からn-1):
i を n - 1 から 1 まで減少させながら、以下を実行する
j に 0 以上 i 以下のランダムな整数を代入する
a[j] と a[i]を交換する
*)
let datel = 9000
let sortlist = Array.init datel (fun i -> i)
let dainyu i d=
  let swap x y z = 
    sortlist.(x) <- sortlist.(y); 
    sortlist.(y) <- z
  in
  if i<>0 then
    swap i (Random.int i) d
;; 
Array.mapi dainyu sortlist;;

let timer f t =
  let x=Sys.time() in
  f t ;
  print_float (Sys.time()-.x) ;
  print_string "\n"
;; 

(*選択ソート*)
let sortlist1 = Array.copy sortlist
let numax = ref sortlist1.(0) 
    
let swap x y =
  let tmp = sortlist1.(x) in
  sortlist1.(x) <- sortlist1.(y);
  sortlist1.(y) <- tmp
let maxindex x y =
  if sortlist1.(x) > sortlist1.(y)then x
  else y 
let rec search mi n = if n > 0 then
    begin search (maxindex mi n) (n-1) end 
  else mi
    
let rec select i =
  if i > 0 then 
    begin swap i (search 0 i); select (i-1) end 
;; 
timer select (datel-1) ;;
  
(*挿入ソート*)
let sortlist2 = Array.copy sortlist
let swap x y =
  let tmp = sortlist2.(x) in
  sortlist2.(x) <- sortlist2.(y);
  sortlist2.(y) <- tmp
let rec search n=
  if (datel-1) > n then 
    if sortlist2.(n) > sortlist2.(n+1) then
      begin swap (n+1) n; search (n+1)end 
let rec sonyu i=
  if i >= 0 then begin search i; sonyu (i-1)end
;;
timer sonyu (datel-2);;
  
(*バブルソート*)
let sortlist3 = Array.copy sortlist
let swap x y =
  let tmp = sortlist3.(x) in
  sortlist3.(x) <- sortlist3.(y);
  sortlist3.(y) <- tmp 
let flag=ref true
let rec bubble i= 
  if i > 0 then begin
    if sortlist3.(i) < sortlist3.(i-1) then 
      begin
        flag:=true;  swap i (i-1)
      end;
    bubble (i-1) 
  end
         
let rec roop n =
  if flag =ref true then begin flag:=false; bubble n ;roop n end;;
timer roop (datel-1);;


(*バケットソート*)

let sortlist4 = Array.copy sortlist
let bucket = Array.init (datel*2) (fun i -> -1)
let f1 i = bucket.(i) <- i
let rec f2 i = 
  if i >= 0 then
    begin 
      if bucket.(i) <> -1 then sortlist4.(i) <- bucket.(i);
      f2 (i-1)
    end
;;
Array.iter f1 sortlist4 ;; 
timer f2 (datel*2-1);; 

(*クイックソート*)
let sortlist5  = Array.to_list(Array.copy sortlist)
let rec sort = function 
  | [] -> []
  | head::tail -> 
      sort (List.filter (fun x -> x < head) tail)
      @ [head] @
      sort (List.filter  (fun y -> head <= y) tail)
;;
let x = Sys.time();;
sort sortlist5 ;;
print_float(Sys.time()-.x)
