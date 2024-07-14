damier = document.getElementById("damier"); 

damier.addEventListener("click",function(event){
    clicked = event.target;
    if (clicked.tagName == "LI")
    {
        elem_selected = damier.getElementsByClassName("selected");
        if(elem_selected.length === 0)
        {
            clicked.setAttribute("class", clicked.getAttribute("class") + " selected");
        }
        else
        {
            for(elem of elem_selected)
            {
                classes = elem.className.split(" ");
                let newClasses = "";
                for(classname of classes)
                {
                    if(classname!="selected")
                        {newClasses = newClasses +" "+classname;}
                }
                elem.setAttribute("class",newClasses);
            }
            clicked.setAttribute("class", clicked.getAttribute("class") + " selected");
        }

        
    }
})

function ajout_pions(rows){
    rows_occupied = (rows - 2)/2;
    for (i=0;i<rows_occupied;i++)
    {
        row1 = damier.children[i];
        
        
        for(const cell of row1.children)
        {
            if(cell.className == "noir")
            {
                cell.setAttribute("class",cell.getAttribute("class")+" pnoir");
            }
        }
        row2 = damier.children[rows-i-1];
        for(cell of row2.children)
            {
                if(cell.className == "noir")
                {
                    cell.setAttribute("class",cell.getAttribute("class")+" pwhite");
                }
            }
    }
}

function move_pion(pos1, pos2){ //pos 1 and pos2 are coordinates [x,y] //arrays
 let x1 = pos1[0];
 let y1 = pos1[1];

 let x2 = pos2[0];
 let y2 = pos2[1];

 let old_cell = damier.children[x1].children[y1];

 let new_cell = damier.children[x2].children[y2];

 oldCellClasses = old_cell.className.split(" ");
 let classes = "";

 for (classname of oldCellClasses)
 {
    if(classname === "pwhite" || classname === "pnoir")
    {
        pcolor = classname;
    }
    else{
        classes = classes +" "+ classname;
    }
 }
 
 old_cell.setAttribute("class", classes);
 new_cell.setAttribute("class", new_cell.getAttribute("class")+" "+pcolor);


}

function init_damier(rows){
    ajout_pions(rows);
}
 
function string_to_board(board_string,size)
{
    for( i = 0; i < size; i++)
    {
        for( j = 0; j < size; j++)
        {
            damier.children[i].children[j].classList.remove("pwhite","pnoir");
            if(board_string[i][j] == 'b')
            {
                damier.children[i].children[j].classList.add("pnoir");
            }
            else if (board_string[i][j] == 'w')
            {
                damier.children[i].children[j].classList.add("pwhite");
            }
            else {
                ;
            }
        }
    }
}

init_damier(8);


//move_pion([1,0],[7,7]);

setTimeout(() => {
    //string_to_board(["ewewewew","wewewewe","ewewewew","eeeeeeee","eeeeeeee","bebebebe","ebebebeb","bebebebe"],8);
    //console.log(damier.children[0].children[0]);
    //string_to_board(["eeeeeeeeee","eeeeeeeeee","eeeeeeeeee","beeeeeeeee","ebebeeeeee","eeweeebebe","eeeeeeeeee","eeeeeebeee","eeeeeeewee","eeeeeeeeee"],10);
    string_to_board(["ebebebeb","eebebebe","ebebebeb","eeeeeeee","eeebeeee","wewewewe","ewewewew","wewewewe"],8);
}, 2000);
    
