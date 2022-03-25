(function($) {

    var animationTime = 300;

    $('.collapse__head').on('click', function () {
        var $collapse = $(this).parent('.collapse');

        if( ! $collapse.hasClass('collapse--expanded') ) {
            $collapse.addClass('collapse--expanded');
            $collapse.children('.collapse__body').slideDown(animationTime);
        } else {
            $collapse.children('.collapse__body').slideUp(animationTime);
            $collapse.removeClass('collapse--expanded');
        }

    });

})(jQuery);
