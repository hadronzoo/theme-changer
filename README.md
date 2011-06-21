Sunrise/Sunset Theme Changer for Emacs
======================================

Given a location and day/night color themes, this file provides a
`change-theme` function that selects the appropriate theme based on
whether it is day or night. It will continue to change themes at
sunrise and sunset. To install:

Set the location:

    (setq calendar-location-name "Dallas, TX") 
    (setq calendar-latitude 32.85)
    (setq calendar-longitude -96.85)

Specify the day and night themes:

    (require 'theme-changer)
    (change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)

Note: you may need to add the repository path to your loadpath. For
example:

    (add-to-list 'load-path "~/.emacs.d/elisp/theme-changer")
