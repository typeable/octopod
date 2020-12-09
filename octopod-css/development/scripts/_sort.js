(function($) {

    $('.sort').on('click', function () {
       if( ! $(this).hasClass('sort--active') ) {
           $('.sort--active').removeClass('sort--active').removeClass('sort--desc').removeClass('sort--asc');
           $(this).addClass('sort--active').addClass('sort--desc');
       } else {
           if( $(this).hasClass('sort--asc') ) {
               $(this).removeClass('sort--asc').addClass('sort--desc');
           } else if( $(this).hasClass('sort--desc') ) {
               $(this).removeClass('sort--desc').addClass('sort--asc');
           }
       }
    });

})(jQuery);
