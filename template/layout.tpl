<html>
  <head>
    <title>Lynton's Blog</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link href="/static/css/bootstrap.min.css" rel="stylesheet" media="screen">
  </head>
  <body>

  <div class="container">
    <ul class="nav nav-tabs">
        <li class="active"><a href="/">Home</a></li>
        <li><a href="#">Profile</a></li>
        <li><a href="#">Message</a></li>
    </ul>
    <div class="page-header">
        <h1>Lynton Zhang's Blog <small> welcome!</small></h1> 
    </div>
    <div class="row">
        <div class="col-xs-4 col-md-3">
            <ul class="dropdown-menu" role="menu" aria-labelledby="dropdownMenu2" style="display: block; position: static;"> <postNav/> </ul>
        </div>
        <div class="col-xs-8 col-md-9"> 
            <apply-content />
        </div>
    </div>
    </div>


    <footer class="bs-footer" role="contentinfo">
        <div class="container"> <p>Powered by Snap, Heist and Pandoc</p> </div>
    </footer>
  <script src="/static/js/jquery-2.0.3.min.js"></script>
  <script src="/static/js/bootstrap.min.js"></script>
  </body>
</html>
