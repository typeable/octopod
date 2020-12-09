(function($) {
    $(function() {



        /*
         * Page lock
         */

        var documentWidthWithScroll = 0;
        var documentWidthWithoutScroll = 0;
        var scrollWidth = 0;

        var $html = $('html');


        function lockPage() {
            if ( ! $html.hasClass('html-lock') ) {
                documentWidthWithScroll = $(window).width();
                $html.addClass('html-lock');
                documentWidthWithoutScroll = $(window).width();
                $html.css( 'padding-right', (documentWidthWithoutScroll - documentWidthWithScroll) + 'px' );
            }
        }

        function unlockPage() {
            if ( $html.hasClass('html-lock') ) {
                $html.css( 'padding-right', '' );
                $html.removeClass('html-lock');
            }
        }



        /*
         * Popup
         */

        function popupShow(popup){
            lockPage();
            popup.fadeIn(150);
            popup.scrollTop(0);
        }

        function popupHide(popup){
            if( ! popup ) {                //in case of Esc or something
                popup = $('.classic-popup');
            }

            popup.fadeOut(150,function(){  //hide popup THAN unlock page
                unlockPage();
            });
        }

        /* show popup by handler click */

        $('.classic-popup-handler').on('click', function(event) {
            event.preventDefault();
            var popupId = $(this).attr('href') ? $(this).attr('href') : $(this).attr('data-href')
            popupShow( $(popupId) );
            event.stopPropagation();
        });


        /* hide popup by window close click */

        $('.classic-popup__close').on('click', function(event){
            event.preventDefault();
            popupHide( $(this).parents('.classic-popup') );
        });


        /* hide popup by overlay click ( goo.gl/SJG2Hw ) */

        $('.classic-popup').on('click', function(event) {
            if (!$(event.target).closest('.classic-popup__slot').length) {
                popupHide( $('.classic-popup') );
            }
        });


        /* hide popup by Esc press */

        $(document).on('keyup', function(event) {
            if (event.keyCode == 27) {
                popupHide();
            }
        });

    });
})(jQuery);

