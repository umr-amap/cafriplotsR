# =============================================================================
# Script pour créer un logo hexagonal pour CafriplotsR
# =============================================================================

# Installer les packages si nécessaire
if (!require("hexSticker")) install.packages("hexSticker")
if (!require("magick")) install.packages("magick")
if (!require("showtext")) install.packages("showtext")

library(hexSticker)
library(magick)
library(showtext)

# Charger une police Google (optionnel, pour un meilleur rendu)
font_add_google("Montserrat", "montserrat")
showtext_auto()

# -----------------------------------------------------------------------------
# Option 1 : Avec hexSticker (méthode recommandée)
# -----------------------------------------------------------------------------

# Chemin vers le logo original
logo_path <- "vignettes/images/cafriPlot_logo.png"

# Créer le sticker hexagonal
sticker(
  # Image centrale
  subplot = logo_path,
  s_x = 1,           # Position x de l'image (1 = centre)
  s_y = 1.05,        # Position y de l'image (légèrement au-dessus du centre)
  s_width = 0.9,     # Largeur de l'image
  s_height = 0.9,    # Hauteur de l'image


  # Texte du package
  package = "CafriplotsR",
  p_x = 1,           # Position x du texte
  p_y = 0.45,        # Position y du texte (en bas)
  p_size = 15,       # Taille du texte
  p_color = "#FFFFFF", # Couleur du texte (blanc)
  p_family = "montserrat", # Police

  # Hexagone
  h_fill = "#1B4D3E",   # Couleur de fond (vert foncé)
  h_color = "#2E8B57",  # Couleur de la bordure (vert moyen)
  h_size = 1.5,         # Épaisseur de la bordure

  # Options de sortie
  filename = "vignettes/images/CafriplotsR_hex_logo.png",
  dpi = 300            # Résolution
)

cat("Logo hexagonal créé : vignettes/images/CafriplotsR_hex_logo.png\n")

# -----------------------------------------------------------------------------
# Option 2 : Version alternative avec magick (plus de contrôle)
# -----------------------------------------------------------------------------

create_hex_logo_magick <- function() {

  # Charger le logo original
  logo <- image_read(logo_path)

  # Obtenir les dimensions
  info <- image_info(logo)

  # Redimensionner le logo pour qu'il rentre dans l'hexagone
  logo_resized <- logo %>%
    image_resize("400x400") %>%
    image_background("transparent")

  # Créer un hexagone avec SVG
  hex_svg <- '
  <svg width="518" height="600" xmlns="http://www.w3.org/2000/svg">
    <polygon
      points="259,0 518,150 518,450 259,600 0,450 0,150"
      fill="#1B4D3E"
      stroke="#2E8B57"
      stroke-width="8"/>
  </svg>'

  # Créer l'hexagone
  hex <- image_read_svg(hex_svg)

  # Superposer le logo sur l'hexagone
  result <- hex %>%
    image_composite(logo_resized, gravity = "center", offset = "+0-30") %>%
    image_annotate(
      "CafriplotsR",
      size = 45,
      color = "white",
      font = "sans-serif",
      weight = 700,
      gravity = "south",
      location = "+0+60"
    )

  # Sauvegarder
  image_write(result, "vignettes/images/CafriplotsR_hex_logo_v2.png")

  cat("Logo hexagonal (version 2) créé : vignettes/images/CafriplotsR_hex_logo_v2.png\n")

  return(result)
}

# Décommenter pour utiliser la version magick
# create_hex_logo_magick()

# -----------------------------------------------------------------------------
# Option 3 : Version simple sans fond (logo découpé en hexagone)
# -----------------------------------------------------------------------------

create_hex_cutout <- function() {

  logo <- image_read(logo_path)

  # Créer un masque hexagonal
  mask_svg <- '
  <svg width="518" height="600" xmlns="http://www.w3.org/2000/svg">
    <polygon
      points="259,0 518,150 518,450 259,600 0,450 0,150"
      fill="white"/>
  </svg>'

  mask <- image_read_svg(mask_svg) %>%
    image_resize("518x600!")

  # Redimensionner et centrer le logo
  logo_prep <- logo %>%
    image_resize("450x450") %>%
    image_extent("518x600", gravity = "center", color = "transparent")

  # Créer le fond hexagonal
  bg_svg <- '
  <svg width="518" height="600" xmlns="http://www.w3.org/2000/svg">
    <polygon
      points="259,0 518,150 518,450 259,600 0,450 0,150"
      fill="#1B4D3E"
      stroke="#2E8B57"
      stroke-width="6"/>
  </svg>'

  bg <- image_read_svg(bg_svg)

  # Composer l'image finale
  result <- bg %>%
    image_composite(logo_prep, gravity = "center", offset = "+0-40") %>%
    image_annotate(
      "CafriplotsR",
      size = 42,
      color = "white",
      font = "sans-serif",
      weight = 700,
      gravity = "south",
      location = "+0+55"
    )

  image_write(result, "vignettes/images/CafriplotsR_hex_logo_v3.png")

  cat("Logo hexagonal (version 3) créé : vignettes/images/CafriplotsR_hex_logo_v3.png\n")

  return(result)
}

# Décommenter pour utiliser cette version
# create_hex_cutout()

cat("\n=== Instructions ===\n")
cat("1. Exécutez ce script dans RStudio\n")
cat("2. Le logo hexagonal sera créé dans vignettes/images/\n
")
cat("3. Trois versions sont disponibles :\n")
cat("   - Option 1 (hexSticker) : La plus simple, recommandée\n")
cat("   - Option 2 (magick) : Plus de contrôle sur le positionnement\n")
cat("   - Option 3 (magick) : Version avec fond coloré\n")
cat("\nAjustez les paramètres (couleurs, tailles, positions) selon vos préférences.\n")
