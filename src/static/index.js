// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.generationRequest.subscribe(function(ids){
    console.log(ids);
    app.ports.generationResponse.send(ids);
})