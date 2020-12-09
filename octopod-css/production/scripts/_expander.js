(function($) {

    $('.expander').on('click', function (event) {
        $(this).toggleClass('expander--open');
        event.stopPropagation();
    });

})(jQuery);
