# Managing the Taxonomic Backbone with the Interactive App

``` r

library(CafriplotsR)
```

## Introduction

The
[`launch_taxo_backbone_app()`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxo_backbone_app.md)
function provides an interactive Shiny application for managing the
Central African plant taxonomic backbone database. This application
allows you to:

- **Browse and search** taxonomic entries at all levels (family, genus,
  species, etc.)
- **Visualize taxonomic hierarchy**
- **Add new taxa** either manually or by retrieving information from
  TROPICOS database
- **Modify existing taxa** with cascade updates to maintain consistency
  in the hierarchy
- **Manage synonymy** including setting, reversing, and canceling
  synonym relationships
- **View traits** at the taxon level

## Prerequisites

**Important**: This application requires **write permissions** on the
taxa database. Regular users typically have **read-only** access.
Contact your database administrator if you need to manage taxonomic
data.

Before launching the app, ensure you have:

**Database credentials** with appropriate permissions

## Quick Start

Launch the app:

``` r

library(CafriplotsR)
launch_taxo_backbone_app()
```

You’ll be prompted to enter your database credentials. **Do not hardcode
credentials in code**:

``` r

# CORRECT - Interactive prompts
launch_taxo_backbone_app()

# NEVER DO THIS - Credentials in code
# launch_taxo_backbone_app(user = "john.doe", password = "MyP@ssw0rd123")
```

## Main Features

### 1. Browse & Search Taxa

The search interface supports multiple modes:

#### Name Search (any taxonomic level)

Search for family, genus, or species names:

``` r

# Examples of searches:
# - "Fabaceae" → finds family and all taxa in that family
# - "Gilbertiodendron" → finds genus and all species
# - "Gilbertiodendron dewevrei" → finds the species
```

**Search Options:** - **Exact match**: Strict matching
(case-insensitive) - **Fuzzy match** (default): Similarity-based
matching for typos - **Include synonyms of queried taxa**: Also show
taxa that are synonyms of your search term - **Include child taxa**:
Show all descendants (e.g., all species in a genus)

**Synonymy Filter:** - **All taxa**: Shows both accepted names and
synonyms - **Accepted names only**: Filters out synonyms (idtax_good_n
IS NULL) - **Synonyms only**: Shows only synonym entries

#### Structured Search

Use separate fields for genus, species, family, order for more precise
queries.

#### Key Features:

- Results show taxonomic level, scientific name, synonymy status
- Click a row to select and view details
- Selected taxon appears in all other tabs for editing/viewing

### 2. Hierarchy View

Visualize the full taxonomic tree from class down to the selected taxon.

**Features:** - **Breadcrumb path**: Shows the lineage (Class → Order →
Family → Genus → Species) - **Tree visualization**: Nested view with
color-coded levels - **Children summary**: Counts of genera, species,
infraspecific taxa under the selected taxon - **ID display**: Shows
database IDs for reference

**Use Cases:** - Verify that a species is correctly placed in its
genus - Check the full lineage of a taxon - Understand parent-child
relationships

### 3. Add New Taxa

Add new taxonomic entries with automatic parent linking.

**Workflow:** 1. Fill in taxonomic information (genus, species, family,
etc.) 2. Set taxonomic level (family, genus, species, infraspecific) 3.
The app **automatically**: - Finds the appropriate parent taxon -
Creates it if missing (e.g., creates genus entry when adding a
species) - Links via id_parent - Validates the hierarchy

The new entry can be filled based on a match in TROPICOS. If no match is
found, the new entry can be filled manually.

**Adding Morpho-taxa:**

It is possible to add a morpho-taxon (provisional taxonomic name). This
may be needed in several situations:

- A species considered new to science but not yet formally published
- A taxon for which the correct identification is pending (e.g., sterile
  material without flowers or fruits)
- A taxon restricted to a specific locality or plot awaiting formal
  description

We recommend following consistent naming conventions for morpho-taxa, as
these will be visible to all users:

1.  **New species awaiting formal description**: Include the (future)
    author name if known Example: *Strephonema sp.nov. Lachenaud O.*
    (where “sp.nov.” is the species epithet)

2.  **Locality-restricted morpho-taxon**: Add the locality name in the
    species epithet Example: *Diospyros sp.nov. Sao Tomé Nguema*

**Example: Adding a New Species**

``` r

# In the app interface:
# Genus: Gilbertiodendron
# Species epithet: newspecies
# Family: Fabaceae
# Order: Fabales
# Class: Magnoliopsida
# Level: species

# The app will:
# 1. Find or create "Gilbertiodendron" genus entry
# 2. Link the new species to that genus via id_parent
# 3. Populate all flat columns (tax_gen, tax_fam, tax_order, tax_famclass)
```

**Important Notes:** - Always provide complete upper taxonomy - Use
correct spelling and nomenclatural authorities - Check for duplicates
before adding (this is checked in any case)

### 4. Modify Taxa

Edit existing taxonomic entries with automatic cascade updates.

**Simple Modifications:** - Change species epithet, author, publication
year - Update non-hierarchical fields

**Upper Taxonomic Field Changes (with Cascade):**

When you modify an upper field (e.g., changing family order from NA to
“Fabales”), the app:

1.  **Detects descendants** via id_parent (all children, grandchildren,
    etc.)
2.  **Shows warning modal** with list of affected taxa
3.  **Waits for confirmation**
4.  **Executes cascade update**:
    - Updates the parent taxon
    - Finds or creates the upper taxon entry (e.g., “Fabales” order)
    - Updates id_parent to point to it
    - Updates flat columns on all descendants
    - Maintains consistency

**Example: Filling in Missing Order**

``` r

# Scenario: Asteraceae family has tax_order = NA
# You change it to "Asterales"

# The app:
# 1. Finds all genera and species in Asteraceae (via id_parent)
# 2. Shows modal: "This will affect 3,843 descendant taxa"
# 3. On confirmation:
#    - Finds or creates "Asterales" order entry
#    - Sets Asteraceae's id_parent to Asterales
#    - Updates tax_order = "Asterales" on all 3,843 descendants
#    - All done in a transaction (atomic, safe)
```

**NA Value Handling:** The app safely handles NA values in all fields,
preventing crashes.

### 5. Set Synonymy

Manage synonym relationships between taxa.

**Three Modes:**

#### A. Set New Synonymy

Make the selected taxon a synonym of another:

``` r

# 1. Select a taxon (e.g., "Leguminosae")
# 2. Click "Set New Synonymy"
# 3. Enter the accepted name: "Fabaceae"
# 4. The app:
#    - Finds Fabaceae entry
#    - Sets Leguminosae's idtax_good_n to Fabaceae's idtax_n
#    - Shows list of other synonyms that point to the same accepted name
```

#### B. Reverse Synonymy

When the selected taxon is already a synonym, swap it with the accepted
name:

``` r

# Scenario: "Leguminosae" is a synonym of "Fabaceae"
# You want to make "Leguminosae" the accepted name

# 1. Select "Leguminosae"
# 2. Click "Reverse Synonym"
# 3. The app shows:
#    - Current accepted name: "Fabaceae"
#    - Other synonyms that will be redirected
# 4. On confirmation:
#    - "Leguminosae" becomes accepted (idtax_good_n = NULL)
#    - "Fabaceae" becomes synonym pointing to "Leguminosae"
#    - All other synonyms also redirect to "Leguminosae"
```

#### C. Cancel Synonymy

Remove synonym status, making the taxon independent:

``` r

# 1. Select a synonym taxon
# 2. Click "Cancel Synonymy"
# 3. Sets idtax_good_n = NULL (becomes accepted/independent)
```

**Important:** - Prevents synonym chains (A → B → C) - All synonyms must
point directly to an accepted name

### 6. Consistency Check

Validate that flat columns match the id_parent hierarchy.

**What It Checks:**

1.  **Species → Genus**: tax_gen matches parent genus entry
2.  **Genus → Family**: tax_fam matches parent family entry
3.  **Family → Order**: tax_order matches parent order entry
4.  **Order → Class**: tax_famclass matches parent class entry
5.  **Missing Parents**: Taxa with upper fields filled but no id_parent
    link

**Running the Check:**

``` r

# From R console:
con <- connect_cafri()$taxa
issues <- check_hierarchy_consistency(con)

# Fix issues automatically:
check_hierarchy_consistency(con, fix = TRUE)
```

**In the App:** - View inconsistencies by type - See affected taxa with
details - Fix automatically or manually

**Example Issues:** - Family “Asteraceae” has tax_order = “Asterales”
but id_parent = NULL - Species “Coffea arabica” has tax_gen = “Coffea”
but parent is a family entry

### 7. Trait Management

View and add species-level traits to taxa.

**Features:** - View existing traits for selected taxon - Add new trait
measurements - Link to trait definitions - Validate trait values

## Advanced Usage

### Working with Duplicates

If you have duplicate entries (e.g., multiple “Fabaceae” entries):

**Best Practice:** 1. Identify the **correct/primary** entry 2. Make
duplicates **synonyms** of the primary 3. Use “Accepted names only”
filter to work with the primary

**Why Not Delete?** - Maintains data provenance - Preserves historical
links - Can be reversed if needed

### Batch Operations via R Console

For bulk updates, use R functions:

``` r

# Check consistency for all taxa
con <- connect_cafri()$taxa
issues <- check_hierarchy_consistency(con, limit = 1000)

# Fix all missing parents
check_hierarchy_consistency(con, fix = TRUE)

# Update multiple taxa programmatically
# (Be very careful - test on a subset first!)
```

## The Hybrid Taxonomic System

CafriplotsR uses a **HYBRID approach** to store taxonomic data:

### Flat Columns (Denormalized)

- `tax_gen` - Genus name
- `tax_fam` - Family name
- `tax_order` - Order name
- `tax_famclass` - Class name

Used for **fast querying** and **backward compatibility**.

### Hierarchical Structure

- `id_parent` - Points to the parent taxon entry
- Enables **tree navigation** and **hierarchy validation**

### Why Both?

- **Fast queries**: Filter by family without joining tables
- **Consistency validation**: Check that species → genus → family →
  order relationships are correct
- **Flexible updates**: Cascade changes when modifying upper taxa

**The app automatically maintains synchronization between both
systems.**

## Best Practices

### 1. Always Check Before Modifying

- Search for the taxon first
- Verify it’s the correct entry
- Check for synonyms or duplicates

### 2. Understand Cascade Impacts

- Modifying a family affects ALL genera and species in it
- Review the warning modal carefully
- Start with small changes to understand behavior

### 3. Maintain Nomenclatural Standards

- Use correct botanical Latin
- Follow ICBN/ICN rules
- Include author citations when available
- Document sources in notes

### 4. Regular Consistency Checks

- Run weekly or after major updates
- Fix issues promptly
- Document recurring problems

### 5. Backup Before Major Changes

- Contact database administrator
- Request a backup before bulk updates
- Test on a development database first

## Troubleshooting

### App Won’t Launch

``` r

# Check database connection
con <- connect_cafri()$taxa
print_connection_status()

# Check permissions
db_diagnostic()
```

### “Permission Denied” Errors

- You need write access to the taxa database
- Contact database administrator
- Most users have read-only access

### “SSL SYSCALL error: EOF detected”

- Clean up connections before closing app
- The app does this automatically via session cleanup

### Hierarchy View Shows “Could not load hierarchy”

- The taxon may not have id_parent set
- Run consistency check to populate missing parents
- Contact administrator if issue persists

### Cascade Update Fails

- Check transaction log for errors
- Verify all descendants have valid data
- Ensure no circular references exist

## Session Cleanup

The app automatically cleans up database connections when closed. If you
need to manually clean up:

``` r

# Clean up all connections
cleanup_connections()
```

## Getting Help

For issues or questions:

1.  Check this vignette and CLAUDE.md
2.  Run diagnostics:
    [`db_diagnostic()`](https://umr-amap.github.io/cafriplotsR/reference/db_diagnostic.md)
3.  Contact database administrator
4.  File an issue: <https://github.com/anthropics/claude-code/issues>

## Related Vignettes

- **Database Connections Guide**: Connection management and credentials
- **Using the Taxonomic Name Standardization App**: Match external names
  to backbone
- **Updating Data**: General data modification workflows

------------------------------------------------------------------------

**Note**: This app modifies the taxonomic backbone. Changes affect all
users and all data linked to these taxa. Always verify changes carefully
before confirming.
