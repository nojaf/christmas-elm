// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.generationRequest.subscribe(function(ids){
    app.ports.generationResponse.send(createResponse(ids));
})

function shuffle(array) {
    var currentIndex = array.length, temporaryValue, randomIndex ;

    // While there remain elements to shuffle...
    while (0 !== currentIndex) {

        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array;
}

function createResponse(persons){
    var result = shuffle(persons.slice(0));

    var duplicates = result.some(function(id, index){
       return id === persons[index];
    });

    return duplicates ? createResponse(persons) : result;
}