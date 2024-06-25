// selection handling
let damier_taille = 3
damier = document.getElementById("damier");
damier.addEventListener("click",function(event){
    clicked = event.target;
    if (clicked.tagName == "LI")
    {
        selected = document.getElementsByClassName("selected");
        pcolor = (  selected.classList.contains("pwhite"))?"white":"noir";
        if(selected.length===0){
            clicked.setAttribute("class", clicked.getAttribute("class") + " selected");
        }
        else{
        possible_pos = possible_positions(pion_to_coord(selected[0]) ,false,pcolor);
        console.log(possible_pos);
        move_pion(selected[0],clicked);
        selected[0].classList.remove("selected");
        //clicked.setAttribute("class", clicked.getAttribute("class") + " selected");
    }
    }
});
function ajout_pions_ligne(row,color){
    for (elem of row.children){
        if (elem.getAttribute("class").split(" ")[0]=="white"){
            elem.setAttribute("class", elem.getAttribute("class") + " p"+color);
        }
    }
}
function ajout_pions_blancs(n_rows){
    //n_rows<damier.children.length;
damier = document.getElementById("damier").children;
for (elem of damier){
    if(n_rows==0){
        break;
    }
    if (elem.tagName == "UL"){
        ajout_pions_ligne(elem,"white");
        n_rows--;
    }
}
}
function ajout_pions_noirs(n_rows){
    //n_rows<damier.children.length;
damier = document.getElementById("damier").children;
for (i=damier.length-1; i>=0; i--){
    elem=damier[i];
    if(n_rows==0){
        break;
    }
    if (elem.tagName == "UL"){
        ajout_pions_ligne(elem,"noir");
        n_rows--;
    }
}
}
function ajout_pions(){
    ajout_pions_blancs(damier_taille);
    ajout_pions_noirs(damier_taille);
}
ajout_pions();
function move_pion(current, target){
   let class_current = current.getAttribute("class").split(" ");
   let class_target = target.getAttribute("class").split(" ");
   let p_current = (class_current.indexOf("pwhite")!=-1)?"pwhite":(class_current.indexOf("pnoir")!=-1)?"pnoir":"";
   let p_target = (class_target.indexOf("pwhite")!=-1)?"pwhite":(class_target.indexOf("pnoir")!=-1)?"pnoir":"";
  //console.log("before",p_current,p_target);
   if(p_target===""){
    if(p_current!=""){
        current.classList.remove(p_current);
        target.classList.add(p_current);
        
    }
   }
   else{
    if(p_target != p_current){
        if(p_current!=""){
            current.classList.remove(p_current);
            target.classList.add(p_current);
            target.classList.remove(p_target);
        }
        
    }
   } 
   //console.log("after",p_current,p_target);
   
}

function possible_positions(selected, dame, color_pion){
    let selected_x = selected[0];
    let selected_y = selected[1];
    if(dame){
        return [];
    }
    else{
        if(color_pion=="noir")
        {
            return[[selected_x-1,selected_y+1],[selected_x-1,selected_y-1]];
        }
        else{

            return[[selected_x+1,selected_y+1],[selected_x+1,selected_y-1]]
        }
    }
   
}
function pion_to_coord(pion){
    return [parseInt(pion.id[1]),parseInt(pion.id[2])]
}
/*
function check_available(damier,selected, dame, color_pion){
    let res;
    let possible_pos = possible_positions(selected,dame,color_pion);
    for (pos in possible_pos){
        if
    }
}
*/
function play(damier,player){
    return ;
}
function init(){
    //a function to initialize all things. (ajout_pions...)
}