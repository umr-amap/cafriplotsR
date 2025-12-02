# Quick Start: Adding Plot Features

## 🎯 What was created

A new user-friendly function `add_plot_features()` to add subplot features (team members, census dates, plot characteristics) to existing plots.

## 📦 Installation

The function is now part of the package. Just reload:

```r
devtools::load_all()  # Or restart R and reload package
```

## 🚀 Basic Usage (3 steps!)

```r
library(CafriplotsR)

# Step 1: Prepare your data
plot_features <- data.frame(
  plot_name = c("Plot-A", "Plot-B", "Plot-C"),
  team_leader = c("John Doe", "Jane Smith", "Bob Wilson"),
  principal_investigator = c("Dr. Smith", "Dr. Smith", "Dr. Jones")
)

# Step 2: Preview (DRY RUN - always do this first!)
add_plot_features(data = plot_features, dry_run = TRUE)

# Step 3: Actually import
add_plot_features(data = plot_features, dry_run = FALSE)
```

## 📚 Documentation

### Files Created:
1. **`R/add_plot_features_user_friendly.R`** - Main function implementation
2. **`vignettes/adding-plot-features.Rmd`** - Comprehensive tutorial (40+ examples!)
3. **`examples/example_add_plot_features.R`** - 8 complete working examples
4. **`man/add_plot_features.Rd`** - R documentation (auto-generated)

### View Documentation:
```r
?add_plot_features
vignette("adding-plot-features")
```

### Run Examples:
```r
source("examples/example_add_plot_features.R")
```

## ✨ Key Features

### 1. **Intelligent Column Mapping**
The function automatically maps your column names:
- `PI` → `principal_investigator`
- `TeamLead` → `team_leader`
- `leader` → `team_leader`

### 2. **Auto-detects Plot ID Column**
Works with either:
- `plot_name` (plot names as strings)
- `id_liste_plots` (plot IDs as integers)

### 3. **Handles Multiple People**
Comma-separated names are automatically split:
```r
team_leader = "John Doe, Jane Smith, Bob Wilson"
# Becomes 3 separate records
```

### 4. **Safe by Default**
- `dry_run = TRUE` by default
- Comprehensive validation
- Clear error messages

### 5. **Interactive Mode**
Guides you through column mapping if auto-detection fails

## 🎓 Learn More

**See the full vignette** for:
- Step-by-step workflow
- Advanced usage (custom mappings, census dates, etc.)
- 8 complete scenarios
- Troubleshooting guide
- Best practices

```r
# Open vignette in browser
vignette("adding-plot-features", package = "CafriplotsR")

# Or view Rmd source
file.edit("vignettes/adding-plot-features.Rmd")
```

## 📋 What's Available?

See all subplot feature types:
```r
available_features <- subplot_list()
View(available_features)
```

Common feature types:
- `team_leader`
- `principal_investigator`
- `data_manager`
- `data_provider`
- `additional_people`
- `census_date` (with year/month/day)
- `plot_area`
- `vegetation_type`

## 🔧 Next Steps

### Test with your data:
```r
# 1. Load your data
my_data <- read.csv("my_plot_features.csv")

# 2. Preview
add_plot_features(data = my_data, dry_run = TRUE)

# 3. Import
add_plot_features(data = my_data, dry_run = FALSE)
```

### Want a Shiny app?
Let me know and I'll create an interactive web interface for adding plot features!

## 💡 Tips

1. **Always dry run first**: `dry_run = TRUE`
2. **Check plots exist**: Use `query_plots()` to verify plot names
3. **Use interactive mode**: Set `interactive = TRUE` if unsure about column mapping
4. **Verify after import**: Query back to confirm features were added

## 🐛 Troubleshooting

### "Plots not found in database"
```r
# Check plot names match exactly
query_plots(plot_name = "YourPlotName", exact_match = TRUE)
```

### "Could not auto-map column"
```r
# Use interactive mode
add_plot_features(data = my_data, interactive = TRUE, dry_run = TRUE)

# Or provide explicit mapping
mapping <- list(MyColumn = "team_leader")
add_plot_features(data = my_data, column_mapping = mapping, dry_run = TRUE)
```

### "Invalid feature type"
```r
# Check available features
subplot_list()
```

## 📞 Need Help?

1. Read the vignette: `vignette("adding-plot-features")`
2. Check examples: `source("examples/example_add_plot_features.R")`
3. View function help: `?add_plot_features`

---

**Ready to use!** The function is fully implemented, documented, and tested. 🎉
