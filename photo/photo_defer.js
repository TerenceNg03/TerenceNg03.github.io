var path = window.location.pathname;
var page = path.split("/").pop();
for (let link of document.body.getElementsByTagName("a")){
	href_page = link.href.split("/").pop();
	console.log(href_page, page);
	if(href_page==page){
		link.classList.add("page_index_current");
	}
}

