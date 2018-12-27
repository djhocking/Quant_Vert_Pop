
/**
 * Add MathJax support to HTML using settings variables defined in the module
 */
Drupal.behaviors.mathjax = function(context) {
  var mathjax = Drupal.settings.mathjax;
  // From http://www.mathjax.org/resources/docs/?dynamic.html
  var script = document.createElement("script");
  script.type = "text/javascript";

  if (mathjax.path=='cdn') {
     //script.src  = "https://d3eoax9i5htok0.cloudfront.net/mathjax/1.1-latest/MathJax.js";
     //script.src  = "http://cdn.mathjax.org/mathjax/latest/MathJax.js"; // unsafe clear connection
     //script.src  = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"; // unsafe clear connection
     //script.src  = "https://onlinecourses.science.psu.edu/libraries/mathjax/MathJax.js"; // unsafe clear connection
     //script.src  = "https://dev.science.psu.edu/mirror/libraries/mathjax/MathJax.js"; // unsafe clear connection
     //script.src  = "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"; // unsafe clear connection
     //script.src  = "https://online.science.psu.edu/sites/all/libraries/mathjax/MathJax.js"; // unsafe clear connection
     script.src = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
  }
  else {
    script.src = mathjax.path;
  }

  var config = 'MathJax.Hub.Config({' + 
                 'extensions: ["tex2jax.js", "TeX/AMSmath.js"],' +
                 'jax: ["input/TeX","output/HTML-CSS"],' +
                 'tex2jax: {' +
                   'inlineMath: [ [\'$\',\'$\'], [\'\\\\(\',\'\\\\)\'] ],' +  // look for $...$ and \(...\) as delimiters for inline math
                   'displayMath: [ [\'$$\',\'$$\'], [\'\\\\[\',\'\\\\]\'] ],' + // look for $$...$$ and \[...\] as delimiters for display math
                   'processEscapes: true' +
                 '}' +
               '});' +
               'MathJax.Hub.Startup.onload();';

  if (window.opera) {
    script.innerHTML = config;
  }
  else {
    script.text = config;
  }
  document.getElementsByTagName("head")[0].appendChild(script);
};
