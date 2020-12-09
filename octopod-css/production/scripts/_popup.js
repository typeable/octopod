(function($) {

    $('.popup-handler').on('click', function (event) {
        event.preventDefault();
        var popupId = $(this).attr('href') ? $(this).attr('href') : $(this).attr('data-href')
        $( popupId ).addClass('popup--visible');
        event.stopPropagation();
    });


    /* Закрываем по крестику */

    $('.popup__close').on('click', function(event){
        event.preventDefault();
        $(this).parents('.popup').removeClass('popup--visible');
    });


    /* Закрываем по клику вне ( goo.gl/SJG2Hw ) */

    $('.popup__overlay').on('click', function(event) {
        $('.popup--visible').removeClass('popup--visible');
    });


    /* Закрываем по Esc */

    $(document).on('keyup', function(event) {
        if (event.keyCode === 27) {
            $('.popup--visible').removeClass('popup--visible');
        }
    });


})(jQuery);
