var del          = require('del');
var run          = require('run-sequence');

var gulp         = require('gulp');
var plumber      = require('gulp-plumber');
var stylelint    = require('gulp-stylelint');
var cleanCSS     = require('gulp-clean-css');
var size         = require('gulp-size');
var postcss      = require('gulp-postcss');
var sprites      = require('postcss-sprites').default;
var cssnext      = require('postcss-cssnext');
var base64       = require('gulp-base64');
var svgstore     = require('gulp-svgstore');
var svgmin       = require('gulp-svgmin');
var change       = require('gulp-change');



function addSourcesTimestamp(content) {
    var source = content.split('\n');
    var outputLine = '';
    var result = '';

    var timestamp = Math.round(new Date().getTime() / 1000);


    source.forEach(function (line) {

        if( line.indexOf('rel="stylesheet"') !== -1 ) {
            outputLine = line.replace('.css"', '.css?' + timestamp + '"');
            result += outputLine + '\n';
        }
        else if ( line.indexOf('<script') !== -1 && line.indexOf('src="') !== -1 && line.indexOf('vendors/') === -1) {
            outputLine = line.replace('.js"', '.js?' + timestamp + '"');
            result += outputLine + '\n';
        }
        else {
            result += line + '\n';
        }

    });

    return result;
}

function uncommentGoogleFonts(content) {
    var source = content.split('\n');
    var outputLine = '';
    var result = '';

    source.forEach(function (line) {

        if( line.indexOf('google') !== -1 && line.indexOf('fonts') !== -1 ) {
            outputLine = line;
            outputLine = outputLine.replace('<!--', '');
            outputLine = outputLine.replace('-->', '');
            result += outputLine + '\n';
        }
        else {
            result += line + '\n';
        }

    });

    return result;
}


function symbolsImgToSpriteSvg(content) {

    var source = content.split('\n');
    var outputLine = [];
    var result = '';

    var i;
    var indentString;
    var classString;
    var idString;
    var widthString;
    var heightString;
    var titleString;
    var srcString;
    var pathString;
    var nameString;
    var timestamp = Math.round(new Date().getTime() / 1000);

    source.forEach(function (line) {

        if( line.indexOf('symbols/') !== -1 ) {

            /* get indent */

            for (indentString = '', i = 0; i < line.indexOf('<img'); i++ ) {
                indentString += ' ';
            }


            /* get attributes */

            classString  = line.match( 'class[ \t]*=[ \t]*"[^"]+"');
            idString     = line.match(    'id[ \t]*=[ \t]*"[^"]+"');
            widthString  = line.match( 'width[ \t]*=[ \t]*"[^"]+"');
            heightString = line.match('height[ \t]*=[ \t]*"[^"]+"');
            titleString  = line.match( 'title[ \t]*=[ \t]*"[^"]+"');

            classString  = classString  ? classString[0]  : null;
            idString     = idString     ? idString[0]     : null;
            widthString  = widthString  ? widthString[0]  : null;
            heightString = heightString ? heightString[0] : null;
            titleString  = titleString  ? titleString[0]  : null;


            /* get path and name */

            srcString = line.match('src[ \t]*=[ \t]*"[^"]+"');
            srcString = srcString[0];
            srcString = srcString.replace('src="', '');
            srcString = srcString.replace('"', '');

            nameString = srcString.replace(/^.*[\\\/]/, '');
            nameString = nameString.replace('.svg', '');

            pathString = srcString.replace(nameString + '.svg', '');


            /* write down results */

            outputLine[0] = indentString + '<svg' + ( classString ? ' ' + classString : '') + ( idString ? ' ' + idString : '') + ( widthString ? ' ' + widthString : '') + ( heightString ? ' ' + heightString : '') + '>';
            outputLine[1] = indentString + '    ' +  '<use xlink:href="' + pathString + 'symbols.svg?' + timestamp + '#' + nameString + '"></use>';
            outputLine[2] = indentString + '</svg>';

            result += outputLine[0] + '\n' + outputLine[1] + '\n' + outputLine[2] + '\n';
        }
        else {
            result += line + '\n';
        }

    });

    return result;
}


// Clean up production folder

gulp.task('clean', function() {
  return del('production/*');
});


// Temp: copy

gulp.task('temp', function() {
  return gulp.src('development/temp/**/*')
      .pipe(plumber())
      .pipe(gulp.dest('production/temp/'))
  ;
});


// Content: copy

gulp.task('content', function() {
  return gulp.src('development/content/**/*')
      .pipe(plumber())
      .pipe(gulp.dest('production/content/'))
  ;
});


// Images: copy

gulp.task('images', function() {
  return gulp.src('development/images/**/*')
      .pipe(plumber())
      .pipe(gulp.dest('production/images/'))
  ;
});


// Markups: copy and change symbols <img> to sprite <svg>

gulp.task('markups', function() {
  return gulp.src('development/markups/**/*')
      .pipe(plumber())
      .pipe(change(symbolsImgToSpriteSvg))
      .pipe(change(uncommentGoogleFonts))
      .pipe(change(addSourcesTimestamp))
      .pipe(gulp.dest('production/markups/'))
  ;
});


// Layouts: copy and change symbols <img> to sprite <svg>

gulp.task('layouts', function() {
  return gulp.src('development/layouts/**/*')
      .pipe(plumber())
      .pipe(change(symbolsImgToSpriteSvg))
      .pipe(change(uncommentGoogleFonts))
      .pipe(change(addSourcesTimestamp))
      .pipe(gulp.dest('production/layouts/'))
  ;
});


// Vendors: copy but exclude normalize

gulp.task('vendors', function() {
  return gulp.src([
      '!development/vendors/normalize/**/*',
      '!development/vendors/normalize/',
      'development/vendors/**/*'
  ])
      .pipe(plumber())
      .pipe(gulp.dest('production/vendors/'))
  ;
});


// Scripts: copy

gulp.task('scripts', function() {
  return gulp.src('development/scripts/**/*')
      .pipe(plumber())
      .pipe(gulp.dest('production/scripts/'))
  ;
});


// Symbols

gulp.task('symbols', function() {
    return gulp.src('development/symbols/*.svg')
        .pipe(plumber())
        .pipe(svgmin())
        .pipe(svgstore())
        .pipe(gulp.dest('production/symbols/'));
});


// Styles: concat, add prefixes, compress, copy

gulp.task('styles', function() {

  var spritesOptions = {
    stylesheetPath: 'production/styles',
    spritePath: 'production/sprites',
    retina: 'true',
    filterBy: function(image) {
      // Allow files from /sprites/ only
      if (!/\/sprites\//.test(image.url)) {
        return Promise.reject();
      }
      return Promise.resolve();
    }
  };

  var processors = [
    sprites(spritesOptions),
    cssnext({
        'browsers': 'last 5 versions' // for autoprefixer and features list
    })
  ];

  return gulp.src([
    'development/styles/style.css'
  ])
      .pipe(plumber())
      .pipe(cleanCSS({
        advanced: false,
        keepSpecialComments: 0
      }))
      .pipe(postcss(processors))
      .pipe(base64({
        // Allow files from /vectors/ only
        exclude: ['/sprite/', '/images/', '/symbols/']
      }))
      .pipe(gulp.dest('production/styles/'))
      .pipe(size())
  ;
});


// lint

gulp.task('lint', function() {

  return gulp.src([
    '!development/styles/style.css',
    'development/styles/**/*.css'
  ])
      .pipe(plumber())
      .pipe(stylelint({
          reporters: [
              {formatter: 'string', console: true}
          ]
      }))
  ;
});


gulp.task('default', function (fn) {
  run('clean', 'temp', 'content', 'images', 'markups', 'layouts', 'vendors', 'scripts', 'symbols', 'styles', 'lint', fn);
});



