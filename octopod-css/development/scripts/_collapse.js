(function($) {

    var animationTime = 300;

    $('.collapse__head').on('click', function () {
        var $collapse = $(this).parent('.collapse');

        if( ! $collapse.hasClass('collapse--expanded') ) {

            /* закрываем все остальные, кроме текущего и его родителей в случае вложенности */
            $('.collapse').not($collapse).not($collapse.parents('.collapse')).find('.collapse__body').slideUp(animationTime);
            $('.collapse').not($collapse).not($collapse.parents('.collapse'));
            $('.collapse').not($collapse).not($collapse.parents('.collapse')).removeClass('collapse--expanded');
            /*/закрываем */

            $collapse.addClass('collapse--expanded');
            $collapse.children('.collapse__body').slideDown(animationTime);
        } else {
            $collapse.children('.collapse__body').slideUp(animationTime);
            $collapse.removeClass('collapse--expanded');
        }

    });

})(jQuery);
