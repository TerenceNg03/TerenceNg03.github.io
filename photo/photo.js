$(function(){
    $.each(document.images, function(){
               var this_image = this;
               var src = $(this_image).attr('src') || '' ;
               if(!src.length > 0){
                   //this_image.src = options.loading; // show loading
                   var lsrc = $(this_image).attr('lsrc') || '' ;
                   if(lsrc.length > 0){
                       var img = new Image();
                       img.src = lsrc;
                       $(img).load(function() {
                           this_image.src = this.src;
                       });
                   }
               }
           });
  });

function insertPhotoIndex() {
	document.write(`
	<div class="page_index">
		<a href="photo.html">[1]</a>
		<a href="photo2.html">[2]</a>
	</div>	
	`);

}

