// Insert catalog
var catalog_div = document.createElement('div')
let real_title = document.getElementsByTagName('h1')[0];
if (real_title != undefined) {
    if (real_title.nextSibling) {
        real_title.parentElement.insertBefore(catalog_div, real_title.nextSibling);
    }
    else {
        real_title.parentElement.appendChild(catalog_div)
    }
    catalog_div.id = 'catalog';
    catalog_div.className = 'catalog';

    /* Generate catalog */
    var content = document.createElement('div');
    content.className = 'catalog-title';
    content.innerHTML = 'Outline';
    catalog_div.appendChild(content);

    var expanded = true;
    var button = document.createElement('div');
    button.className = 'catalog-button';
    button.innerHTML = '-';
    button.id = 'button';
    button.onclick = function () {
        var disp;
        console.log("clicked")
        if (expanded) {
            expanded = false;
            disp = "none";
            button.innerHTML = '+';
            button.style.position = "static";
            button.style.padding = "0em 0.2em"
        } else {
            expanded = true;
            disp = "block";
            button.innerHTML = '-';
            button.style.position = "absolute";
            button.style.padding = "0.6em 1em"
        };
        catalog_div.childNodes.forEach(n => {
            if (n.id != 'button') {
                n.style.display = disp;
            }
        })
    };
    catalog_div.appendChild(button);

    var auto_id = 0;
    $("h2,h3,h4,h5,h6").each(function () {
        let title_div = this;
        let heading = title_div.tagName.toLowerCase();
        title_div.id = auto_id;
        var content = document.createElement('a');
        content.className = 'catalog-' + heading;
        content.href = "#" + auto_id;
        content.innerHTML = title_div.innerHTML;
        catalog_div.appendChild(content);
        auto_id = auto_id + 1;
    });
}
// highlight code
hljs.highlightAll();

