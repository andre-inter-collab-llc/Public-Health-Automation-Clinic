# Generate "Built with AI. Owned by You." infographic
# For use in Appendix D and LinkedIn post
# Uses brand colors from _brand.yml

library(ggplot2)
library(grid)

# Brand colors
brand_blue   <- "#2494f7"
brand_teal   <- "#00a4bb"
brand_navy   <- "#01272f"
brand_dark   <- "#020506"
brand_ivory  <- "#fffff0"
brand_white  <- "#ffffff"
brand_slate  <- "#204d70"

# Flow data: three stages
stages <- data.frame(
  x     = c(1, 2, 3),
  label = c("AI Accelerates\nDevelopment", "Solution is\nPublished", "You Run It\nLocally"),
  sub   = c(
    "GitHub Copilot\nAnthropic \u2022 Google \u2022 OpenAI",
    "Open Source\nR \u2022 Python \u2022 Quarto",
    "Your Machine\nNo AI Required"
  ),
  icon  = c("\U0001F916", "\U0001F4E6", "\U0001F4BB"),
  fill  = c(brand_blue, brand_teal, brand_slate)
)

p <- ggplot(stages) +
  # Background
  theme_void() +
  theme(
    plot.background  = element_rect(fill = brand_navy, color = NA),
    plot.margin      = margin(30, 40, 30, 40),
    text             = element_text(family = "sans")
  ) +
  # Coordinate system

  coord_cartesian(xlim = c(0.3, 3.7), ylim = c(-1.2, 2.8), clip = "off") +
  
  # Title
  annotate("text", x = 2, y = 2.6,
           label = "Built with AI.  Owned by You.",
           color = brand_ivory, size = 10, fontface = "bold",
           family = "sans") +
  
  # Subtitle
  annotate("text", x = 2, y = 2.15,
           label = "Public Health Automation Clinic",
           color = brand_teal, size = 5.5, fontface = "italic",
           family = "sans") +
  
  # Arrows between stages
  annotate("segment", x = 1.35, xend = 1.65, y = 0.8, yend = 0.8,
           color = brand_ivory, linewidth = 1.2,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  annotate("segment", x = 2.35, xend = 2.65, y = 0.8, yend = 0.8,
           color = brand_ivory, linewidth = 1.2,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  
  # Stage boxes
  geom_tile(aes(x = x, y = 0.8), width = 0.65, height = 1.6,
            fill = stages$fill, color = brand_ivory, linewidth = 0.5,
            alpha = 0.9) +
  
  # Stage labels (main)
  geom_text(aes(x = x, y = 1.15, label = label),
            color = brand_white, size = 4.2, fontface = "bold",
            lineheight = 0.9, family = "sans") +
  
  # Stage labels (sub)
  geom_text(aes(x = x, y = 0.4, label = sub),
            color = brand_ivory, size = 3, lineheight = 0.9,
            alpha = 0.85, family = "sans") +
  
  # Bottom tagline
  annotate("text", x = 2, y = -0.65,
           label = "Every solution is transparent, local, and independent of AI once delivered.",
           color = brand_ivory, size = 3.8, family = "sans") +
  
  # Attribution
  annotate("text", x = 2, y = -1.1,
           label = "andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic",
           color = brand_teal, size = 3, family = "sans")

# Save high-resolution PNG suitable for LinkedIn (1200x627 recommended)
ggsave(
  filename = "assets/images/ai-transparency-graphic.png",
  plot     = p,
  width    = 10,
  height   = 5.25,
  dpi      = 192,
  bg       = brand_navy
)

cat("Saved: assets/images/ai-transparency-graphic.png\n")
