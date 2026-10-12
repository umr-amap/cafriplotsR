// Language switch in the navbar.
//
// _pkgdown.yml can only give the "FR" navbar entry a fixed href
// (articles/readme-fr.html), so from any page it led back to the French home.
// French articles are named <article>-fr, so the switch can go to the
// translation of the page being read instead:
//   articles/newsletter.html     <-> articles/newsletter-fr.html
//   index.html                   <-> articles/readme-fr.html
// A page with no translation falls back to the home page of the other language.
// The "Newsletter" entry likewise points to the French newsletter on French pages.

document.addEventListener("DOMContentLoaded", function () {
  var frLink = document.querySelector('.navbar a[href$="articles/readme-fr.html"]');
  if (!frLink) return;

  var root = frLink.href.replace(/articles\/readme-fr\.html$/, "");
  var page = window.location.href.split(/[?#]/)[0].replace(root, "");
  if (page === "" || page.slice(-1) === "/") page += "index.html";

  var article = page.match(/^articles\/(.+)\.html$/);
  var name = article ? article[1] : null;
  var isFrench = name !== null && /-fr$/.test(name);

  var target, fallback;
  if (isFrench) {
    frLink.textContent = "EN";
    frLink.setAttribute("aria-label", "English version");
    fallback = root + "index.html";
    target = name === "readme-fr" ? fallback
      : root + "articles/" + name.replace(/-fr$/, "") + ".html";

    var newsLink = document.querySelector('.navbar a[href$="articles/newsletter.html"]');
    if (newsLink) newsLink.href = root + "articles/newsletter-fr.html";
  } else {
    fallback = frLink.href;
    target = name === null ? fallback : root + "articles/" + name + "-fr.html";
  }

  frLink.href = target;
  if (target === fallback) return;

  // Not every article is translated: check the target exists before using it
  fetch(target, { method: "HEAD" })
    .then(function (response) {
      if (!response.ok) frLink.href = fallback;
    })
    .catch(function () {
      frLink.href = fallback;
    });
});
