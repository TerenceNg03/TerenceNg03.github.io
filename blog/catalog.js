var catalog_div = document.createElement('div')
let real_title = document.getElementsByTagName('h1')[0];
if(real_title.nextSibling){
	real_title.parentElement.insertBefore(catalog_div, real_title.nextSibling);
}
else{
	real_title.parentElement.appendChild(catalog_div)
}
catalog_div.id = 'catalog';
catalog_div.className = 'catalog';

/* Generate catalog */
var content = document.createElement('div');
content.className = 'catalog-title';
content.innerHTML = 'Outline';
catalog_div.appendChild(content);

var auto_id = 0;
$("h2,h3,h4,h5,h6").each(function (){
	let title_div = this;
	let heading = title_div.tagName.toLowerCase();
	title_div.id = auto_id;
	var content = document.createElement('a');
	content.className = 'catalog-'+heading;
	content.href = "#"+auto_id;
	content.innerHTML = title_div.innerHTML;
	catalog_div.appendChild(content);
	auto_id = auto_id + 1;
});
