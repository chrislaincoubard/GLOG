'use strict'

function parseCSV(txt,sep=',') {
    let table = {data:[]};
    // Step 1 - split txt in rows
    const rows = txt.split('\n');
    // Step 2 - extract header rows[0]
    table.header = rows[0].split(sep);
    // Step 3 - for each row, get values
    for (let i=1; i< rows.length-1; i++) {
        const arr = rows[i].split(sep);
        const values = [];
        for (let v of arr) {
            if (isNaN(v)) {
                values.push(v);
            }
            else {
                values.push(parseFloat(v));
            }
        }
        table.data.push(values);
    }
    console.log(table);
    return table;
}

function process(tab) {
    let nb = {};
    for (let ligne of tab['data']) {
        let country = ligne[0];
        let cases = ligne[1];
        if (isNaN(cases)){
            cases = 0;
        }

        if (! nb.hasOwnProperty(country)){
            nb[country] = cases;
        }
        else {
            nb[country] += cases;
        }
    }
    console.log(nb)
    return nb;
}

function dictToNArray(dict) {
    data = list(dict.items())
    an_array = np.array(data)
    return(an_array)
}

function newMap(Map, basemap, layers){
    let object = new Map({
        basemap: basemap,
        layers: layers
    })
    return object;
}

function newView(MapView, map, zoom, center){
    let object = new MapView({
        container: "viewDiv", // Reference to the DOM node that will contain the view
        map: map, // References the map object created in step 3
        zoom: zoom,
        center: center // Longitude, latitude
    })
    return object;
}

function newFeature(FeatureLayer, id){
    let object = new FeatureLayer({
        portalItem: {
            id: id
        }
    })
    return object;
}





//Add Event Listener
let csvBrowse = document.getElementById('browseID');
let tab = csvBrowse.addEventListener('change',uploadAndMap);

async function uploadAndMap(ev) {
    console.log(ev.target);
    console.log(ev.target.files[0]);
    const file = ev.target.files[0];

    const txt = await file.text();
    const tab = parseCSV(txt,';');
    const data = process(tab);

    var info = []
    for (let code of Object.keys(data)){
        info.push({
            field: "Cases",
            label: code,
            format: {
                digitSeparator: true,
                places: 0
            }
        })
    }

    const renderer = {
        type: "simple",
        visualVariables: [{
            type: "color",
            field: "Cases",
            //normalizationField: "TOT_CROP_ACRES",
            legendOptions: {
                title: ""
            },
            // Orange 2 ramp
            stops: [
                {value: 0, color: "#fee6ce", label: "0"},
                {value: 2.5, color: "#fdae6b"},
                {value: 5, color: "#e6550d", label: "5"}
            ]
        }]
    };

    require(["esri/Map", "esri/views/MapView", "esri/layers/FeatureLayer"],(Map, MapView, FeatureLayer) => {
        let worldImagery = newFeature(FeatureLayer, "2b93b06dc0dc4e809d3c8db5cb96ba69");
        let map = newMap(Map, "gray-vector",[worldImagery]);
        let view = newView(MapView, map, 3.5, [2.34, 48.86]);
        
        //worldImagery.renderer = renderer;
        
        // Create a variable referencing the checkbox node
        const countriesLayerToggle = document.getElementById("countriesLayer");
        countriesLayerToggle.addEventListener("change", () => {
            // When the checkbox is checked (true), set the layer's visibility to true
            worldImagery.visible = countriesLayerToggle.checked;
        });
    
    })
}