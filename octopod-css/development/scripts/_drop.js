(function($) {

    $('.drop__handler').on('click', function (event) {
        $(this).parents('.drop').toggleClass('drop--expanded');
        event.stopPropagation();
    });


    /* Закрываем по клику вне ( goo.gl/SJG2Hw ) */

    $(document).on('click', function(event) {
        if (!$(event.target).closest('.drop').length) {
            $('.drop--expanded').removeClass('drop--expanded');
        }
    });


    /* Закрываем по Esc */

    $(document).on('keyup', function(event) {
        if (event.keyCode === 27) {
            $('.drop--expanded').removeClass('drop--expanded');
        }
    });

})(jQuery);
