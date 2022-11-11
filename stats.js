
'use strict'

let graph = document.createElementNS("http://www.w3.org/2000/svg","svg");
graph.setAttribute('viewBox','-100 -100 200 200');
const elts = document.getElementById("graph");
elts.appendChild(graph);

function create(type){
    let elt = document.createElementNS("http://www.w3.org/2000/svg",type/*"svg"*/);
    graph.appendChild(elt);
    return elt; //element svg
}


function attribute(el,key,value=null){
    if (value ==null){
        //get
        value=el.getAttribute(key);
        return value
    }
    else{
        //set
        el.setAttribute(key,value);
        graph.appendChild(el);
    }
    return el;
}

function append_rect(){
    let graph = svg(2000,2000,[-125,-125,2000,2000]);
    document.getElementById("display").appendChild(graph);
    let rec = rect();
    graph.appendChild(rec)
    return parent;
}


function svg(w,h,box =[0,0,w,h]){
    let svg = create('svg');
    let str_box = box.join(" ")
    attribute(svg,'width',w);
    attribute(svg,'height',h);
    attribute(svg,'viewBox',str_box);
    return svg;
}

function rect(){
    let rect = create('rect');
    attribute(rect,'x',200);
    attribute(rect,'y',200);
    attribute(rect,'width',100);
    attribute(rect,'height',50);
    attribute(rect, 'fill', "red")
    return rect;
}