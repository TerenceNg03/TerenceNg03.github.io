function insertLogo(cata='') {
	document.write(`
	<div class="HeadTitle">
		<div class="HeadName`)
	if(cata==''){
		document.write(`NoANI`)
	}
	document.write(`">
				<svg class="Headsvg" width="315px" height="90px" preserveAspectRatio="none" viewBox="0 0 315 90">
					<style>
						.head {
							font: 60px sans-serif;
							text-shadow: 0 0 .2em #AAA, 0 0 .5em #CCC;
							stroke-linejoin: round;
							text-anchor: middle;
							fill: white;
							stroke: #555;
							stroke-width: 2.5px;
						}
					</style>
					<text class="head" x="160" y="70">Terence Ng</_text>
				</svg>
		</div>`)
	if(cata){
		document.write(`
		<div class="cata">
			<span class='HeadSplit'>|</span>
			<span class='HeadPart'>`,cata,`</span>
		</div>
			`)
	}
	document.write(`
	</div>
		`)
}

function insertNavbar(base='') {
	document.write(`
	<div class="navbarContainer">
		<div class="navbar">
			<a href="`,base,`index.html">Home</a>
			<a href="`,base,`blog/blog.html">Blogs</a>
			<a href="`,base,`photo/photo.html">Photos</a>
			<a href="`,base,`prose/prose.html">Proses</a>
			<a href="`,base,`poem/poem.html">Poems</a>
			<a href="#About">About</a>
		</div>
	</div>`);
}

function insertFooter() {
	document.write(`
	<div class="footerContainer">
		<div class="footer">
		<div class="Quote">
			<p>永遠太遠 · 無謂太早 · 分對或錯</p>
		</div>
			<div class="realfooter" id="About">
				<div class="copyright">
					Original Photos & Contents
				</div>
				<div class="copyright">
					<span>Copyright &copy;2021 TerenceNg&nbsp;&nbsp;All Rights Reserved.</span> 
				</div>
			<div class="ContactMe">
				<a href="https://github.com/TerenceNg03">Github Profile</a> | <a href="mailto: notexist@notexist.com">Contact Author</a>
			</div>
			</div>
		</div>
	</div>
`)
}


document.write(`
	<meta name="viewport" content="width=device-width, initial-scale=1">
`)
