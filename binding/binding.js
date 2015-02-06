<style>
	#map {
		width: 800px;
		height: 500px;
	}

	.info {
		padding: 6px 8px;
		font: 14px/16px Arial, Helvetica, sans-serif;
		background: white;
		background: rgba(255,255,255,0.8);
		box-shadow: 0 0 15px rgba(0,0,0,0.2);
		border-radius: 5px;
	}
	.info h4 {
		margin: 0 0 5px;
		color: #777;
	}

	.legend {
		text-align: left;
		line-height: 18px;
		color: #555;
	}
	.legend i {
		width: 18px;
		height: 18px;
		float: left;
		margin-right: 8px;
		opacity: 0.7;
	}
</style>

<link rel="stylesheet" href="http://cdn.leafletjs.com/leaflet-0.5.1/leaflet.css" />
<!--[if lte IE 8]>
    <link rel="stylesheet" href="http://cdn.leafletjs.com/leaflet-0.5.1/leaflet.ie.css" />
<![endif]-->
<script src="http://cdn.leafletjs.com/leaflet-0.5.1/leaflet.js"></script>

<div id="map" class="leaflet-container leaflet-fade-anim" tabindex="0" style="position: relative;">
<script>

  // PARTE DO SHINY
  var mapOutputBinding = new Shiny.OutputBinding();
  $.extend(mapOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-maps-output');
    },
    renderValue: function(el, data) {
            
      //remove the old graph
      var div = document.getElementById("map");      
      div.parentNode.removeChild(div);
      
      var divNew = document.createElement("div");
      divNew.setAttribute('id', 'map');
      divNew.setAttribute('class', 'leaflet-container leaflet-fade-anim');
      divNew.setAttribute('tabindex', '0');
      divNew.setAttribute('style', 'position: relative; width: 800px; height: 600px;');
      el.appendChild(divNew);

      // PARTE DA APLICACAO
      var statesData = "";
      statesData = jQuery.parseJSON(data);

      var map = L.map('map').setView([-17, -55], 4);
    
      var cloudmade = L.tileLayer('http://{s}.tile.cloudmade.com/{key}/{styleId}/256/{z}/{x}/{y}.png', {
			  attribution: 'Map data &copy; 2011 OpenStreetMap contributors, Imagery &copy; 2011 CloudMade',
			  key: '40804541f1d046d68d10013b07efabad',
			  styleId: 22677
		  }).addTo(map);
    
    	// control that shows state info on hover
    	var info = L.control();
    
    	info.onAdd = function (map) {
    		this._div = L.DomUtil.create('div', 'info');
    		this.update();
    		return this._div;
    	};
    
    	info.update = function (prop) {
    		this._div.innerHTML = '<h4>Justiça em Números</h4>' +  (prop ?
    			'<b>' + prop.ESTADO + '</b><br />' + prop.varCont + ''
    			: 'Coloque o mouse sobre um estado');
    	};
    	info.addTo(map);
      
      // get color depending on variable value
    	function getColor(d) {
        cats = statesData.features[0].properties.levels;
    		return d === cats[4] ? '#800026' :
    		       d === cats[3] ? '#E31A1C' :
    		       d === cats[2] ? '#FD8D3C' :
    		       d === cats[1] ? '#FEB24C' :
    		       d === cats[0] ? '#FFEDA0' :
      	                       '#000000';
                               //'';
               //d === cats[5]   ? '' :
    		       //d === cats[6]   ? '' :
               // #FFEDA0 #FEB24C  #FED976 #800026 #BD0026 #E31A1C #FC4E2A #FD8D3C
    	}
    
      
      var marker = L.marker([51.5, -0.09]).addTo(map);
      
    	function style(feature) {
    		return {
    			weight: 2,
    			opacity: 1,
    			color: 'white',
    			dashArray: '3',
    			fillOpacity: 0.7,
    			fillColor: getColor(feature.properties.varCat)
    		};
    	}
    
    	function highlightFeature(e) {
    		var layer = e.target;
    
    		layer.setStyle({
    			weight: 5,
    			color: '#666',
    			dashArray: '',
    			fillOpacity: 0.7
    		});
    
    		if (!L.Browser.ie && !L.Browser.opera) {
    			layer.bringToFront();
    		}
    
    		info.update(layer.feature.properties);
    	}
    
    	var geojson;
    
    	function resetHighlight(e) {
    		geojson.resetStyle(e.target);
    		info.update();
    	}
    
    	function zoomToFeature(e) {
    		map.fitBounds(e.target.getBounds());
    	}
    
    	function onEachFeature(feature, layer) {
    		layer.on({
    			mouseover: highlightFeature,
    			mouseout: resetHighlight,
    			click: zoomToFeature
    		});
    	}
    
    	geojson = L.geoJson(statesData, {style: style, onEachFeature: onEachFeature}).addTo(map);
    
    	map.attributionControl.addAttribution('<a href="http://cnj.jus.br/images/pesquisas-judiciarias/Base_de_dados/Base_de_dados.zip">Dados do Justiça em Números</a>');
     
    	var legend = L.control({position: 'bottomright'});
    
    	legend.onAdd = function (map) {
    
    		var div = L.DomUtil.create('div', 'info legend'),
    			cats = statesData.features[0].properties.levels;
    			labels = [];
    
    		for (var i = 0; i < cats.length; i++) {    
    			labels.push('<i style="background:' + getColor(cats[i]) + '"></i> ' + cats[i] + '<br />');
    		}
    
    		div.innerHTML = labels.join('<br>');
    		return div;
        
    	};
    
    	legend.addTo(map);
  
  // COLOCAR COISAS ACIMA DISSO
    }
  });
  
  // PARTE DO SHINY
  Shiny.outputBindings.register(mapOutputBinding, 'mapaBR');

</script>
</div>
