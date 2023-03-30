$( document ).ready(function() {
  
  // convert the JSON to a JavaScript object: https://bobbyhadz.com/blog/javascript-remove-null-values-from-object
  function removeNull(JSONObj){
    for(var i = 0; i < JSONObj.length; i++){
      var object = JSONObj[i];
      for(var property in object){
        if(object[property] === null){
          delete object[property];
        }
      }
    }
    return JSONObj
  };
  
  // a function to process the R data frame pedigree object and convert it into pedigreeJS readable
  Shiny.addCustomMessageHandler("createPedJSHandler", function(pedJSON) {
    
    // remove null values from the JSON so that pedigreejs will accept it
    var dataset = removeNull(pedJSON);
    
    // set the pedigreejs customization options
		var opts = {
			'targetDiv': 'pedjsTree',
			'btn_target': 'pedjsButtons',
			'width': 550,
			'height': 600,
			'symbol_size': 35,
			'store_type': 'array',
			'zoomIn': 3.0,
			'zoomOut': 3.0,
			'zoomSrc': ['button'], 
			'diseases': [
        {'type': 'Brain_cancer', 'colour': '#FDAC53'},
        {'type': 'Breast_cancer', 'colour': '#9BB7D4'},
        {'type': 'Cervical_cancer', 'colour': '#B55A30'},
        {'type': 'Colorectal_cancer', 'colour': '#F5DF4D'},
        {'type': 'Endometrial_cancer', 'colour': '#0072B5'},
        {'type': 'Gastric_cancer', 'colour': '#A0DAA9'},
        {'type': 'Kidney_cancer', 'colour': '#E9897E'},
        {'type': 'Leukemia_cancer', 'colour': '#00A170'},
        {'type': 'Melanoma_cancer', 'colour': '#926AA6'},
        {'type': 'Ovarian_cancer', 'colour': '#D2386C'},
        {'type': 'Osteosarcoma_cancer', 'colour': '#34568B'},
        {'type': 'Pancreas_cancer', 'colour': '#CD212A'},
        {'type': 'Prostate_cancer', 'colour': '#FFA500'},
        {'type': 'Small_Intestine_cancer', 'colour': '#56C6A9'},
        {'type': 'Soft_Tissue_Sarcoma_cancer', 'colour': '#4B5335'},
        {'type': 'Thyroid_cancer', 'colour': '#798EA4'},
        {'type': 'Urinary_Bladder_cancer', 'colour': '#FA7A35'},
        {'type': 'Hepatobiliary_cancer', 'colour': '#00758F'},
        {'type': 'Contralateral_cancer', 'colour': '#EDD59E'},
			],
      'edit': false,
      'labels': ['age'],
			'font_size': '1.1em',
			'font_family': 'times',
			'DEBUG': false
		};
		
		// code to include as shown here: https://ccge-boadicea.github.io/pedigreejs/
		var local_dataset = pedigreejs.pedcache.current(opts);
		if (local_dataset !== undefined && local_dataset !== null) {
		 	opts.dataset = local_dataset;
		} else {
		  opts.dataset = dataset;
		}
		opts= pedigreejs.pedigreejs.build(opts);
		
		// return the JSON string output from pedigreejs
		getpedigree = function(){
		  Shiny.setInputValue("pedJSJSON", JSON.stringify(pedigreejs.pedcache.current(opts)));
		};
		
  }); // end of Shiny.addCustomMessageHandler
  
  // function to update the pedigree for pedigreeJS
  Shiny.addCustomMessageHandler("updatePedJSHandler", function(pedJSON) {
    
    // remove null values from the JSON so that pedigreejs will accept it
    var dataset = removeNull(pedJSON);
    
    // set the pedigreejs customization options
		var opts = {
			'targetDiv': 'pedjsTree',
			'btn_target': 'pedjsButtons',
			'width': 550,
			'height': 600,
			'symbol_size': 35,
			'store_type': 'array',
			'zoomIn': 3.0,
			'zoomOut': 3.0,
			'zoomSrc': ['button'], 
			'diseases': [
        {'type': 'Brain_cancer', 'colour': '#FDAC53'},
        {'type': 'Breast_cancer', 'colour': '#9BB7D4'},
        {'type': 'Cervical_cancer', 'colour': '#B55A30'},
        {'type': 'Colorectal_cancer', 'colour': '#F5DF4D'},
        {'type': 'Endometrial_cancer', 'colour': '#0072B5'},
        {'type': 'Gastric_cancer', 'colour': '#A0DAA9'},
        {'type': 'Kidney_cancer', 'colour': '#E9897E'},
        {'type': 'Leukemia_cancer', 'colour': '#00A170'},
        {'type': 'Melanoma_cancer', 'colour': '#926AA6'},
        {'type': 'Ovarian_cancer', 'colour': '#D2386C'},
        {'type': 'Osteosarcoma_cancer', 'colour': '#34568B'},
        {'type': 'Pancreas_cancer', 'colour': '#CD212A'},
        {'type': 'Prostate_cancer', 'colour': '#FFA500'},
        {'type': 'Small_Intestine_cancer', 'colour': '#56C6A9'},
        {'type': 'Soft_Tissue_Sarcoma_cancer', 'colour': '#4B5335'},
        {'type': 'Thyroid_cancer', 'colour': '#798EA4'},
        {'type': 'Urinary_Bladder_cancer', 'colour': '#FA7A35'},
        {'type': 'Hepatobiliary_cancer', 'colour': '#00758F'},
        {'type': 'Contralateral_cancer', 'colour': '#EDD59E'},
			],
      'edit': false,
      'labels': ['age'],
			'font_size': '1.1em',
			'font_family': 'times',
			'DEBUG': false
		};
    
		opts.dataset = dataset;
		opts= pedigreejs.pedigreejs.rebuild(opts);
  });
}); // end of document


