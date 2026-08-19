# Linking Herbarium Specimens to Inventory Individuals

## Introduction

### The Challenge: Disconnected Taxonomic Knowledge

Forest inventory data and herbarium specimen data often exist in
separate silos. Field botanists collect specimens from individual trees
during plot censuses, but the connection between the voucher specimen
and the measured tree is typically recorded only in field notebooks.
When taxonomists later revise the specimen’s identification in the
herbarium, this improved knowledge doesn’t automatically flow back to
the ecological dataset.

**This creates a fundamental problem**: Your inventory data can become
taxonomically outdated even though the correct identification exists
somewhere in a herbarium database.

### The Solution: Formal Specimen-Individual Links

The CafriplotsR package solves this by creating **persistent,
database-level connections** between:

- **Individual trees** in your forest plots (with DBH, height,
  coordinates, etc.)
- **Herbarium specimens** collected from those exact individuals (with
  expert-verified taxonomy)

Once linked, taxonomic updates to specimens automatically propagate to
your inventory data—forever.

## How It Works: Step-by-Step Workflow

### Prerequisites

Before linking specimens, ensure you have:

1.  **Database access** with appropriate credentials
2.  **Herbarium reference data** in your individuals table (see columns
    below)
3.  **Specimens already entered** in the herbarium database

**Understanding the two types of herbarium columns:**

The system uses two columns to track different types of
specimen-individual relationships:

- **`herbarium_nbe_type`**: The ACTUAL tree where the herbarium specimen
  was physically collected
  - Direct evidence: “This specimen was collected FROM this tree”
  - High confidence link
  - Link type: `type_individual`
- **`herbarium_nbe_char`**: Other trees that were field-identified as
  the SAME SPECIES as the collected specimen
  - Indirect evidence: “We believe this tree is the same species as the
    specimen tree”
  - Lower confidence (based on field identification assumption)
  - Link type: `referenced_individual`

**Rationale: Extending specimen utility**

Since it’s impractical to collect herbarium specimens from every single
tree in a forest inventory, this system maximizes the value of collected
specimens:

1.  Collect a specimen from one representative individual (recorded in
    `herbarium_nbe_type`)
2.  In the field, identify other trees as the same species (recorded in
    `herbarium_nbe_char`)
3.  Link all these trees to the one specimen
4.  When taxonomists revise the specimen → all linked trees inherit the
    updated identification

**The trade-off**: Links via `herbarium_nbe_char` are less reliable
because they depend on the accuracy of field identification. However,
they greatly extend specimen utility by providing taxonomic updates to
many more trees than would otherwise be possible.

### Step 1: Launch the Linking Application

``` r

library(CafriplotsR)

# Launch the interactive specimen linking app
launch_individual_specimen_linking_app()
```

The app will prompt you for database credentials if not already
connected.

### Step 2: Select Plot(s) and Filter Individuals

The app starts by loading all individuals from your accessible plots
that have herbarium reference information.

**What happens:** - Reads the `herbarium_nbe_char` column from
`data_individuals` table - This column typically contains collector name
and specimen number (e.g., “Dauby 123”, “Smith G. 456”) - Displays a
summary table showing which individuals have specimen references

**Your actions:** - Review the individuals with herbarium information -
Optionally filter by plot, collector, or taxonomic group - The table
highlights individuals with specimen references (green background)

### Step 3: Parse Herbarium References

The app automatically extracts collector names and specimen numbers from
the text references.

**What happens:** - Parses `herbarium_nbe_char` using pattern matching -
Handles various formats: “Dauby 123”, “G. Dauby 123”, “Dauby, G. 123” -
Matches collector names to the `table_colnam` database (people lookup
table) - Extracts specimen numbers as integers

**Result:** - A table showing parsed collector IDs and specimen
numbers - Any unparseable references are flagged for review

### Step 4: Retrieve Matching Specimens

The app queries the herbarium database to find specimens matching the
parsed references.

**What happens:** - Groups by collector and gets min/max specimen number
range - Queries the `specimens` table using batch retrieval (very
fast!) - For example: Instead of 200 individual queries, makes ~3
queries (one per collector)

**Result:** - A table of potential specimen matches - Shows specimen ID,
collector, number, and current taxonomic identification

**Performance note:** The batch query optimization means even large
linking operations (200+ individuals) complete in seconds.

### Step 5: Validate Taxonomic Concordance

Before creating links, the app validates that specimen and individual
identifications are compatible.

**What happens:** - Retrieves taxonomy for both individuals and
specimens from the taxa database - Constructs full taxonomic names
(genus + species + infraspecific) - Compares at multiple levels: -
**Exact match**: Same species (idtax_n matches) - **Same genus**:
Different species but same genus - **Same family**: Different genus but
same family - **Different family**: Potential misidentification

**Visual indicators:** - ✓ Green = Exact match (auto-approve
recommended) - Yellow = Same genus (review recommended) - Orange = Same
family (review required) - Red = Different family (⚠ likely error,
manual verification needed)

**Your actions:** - Review the validation table - Click “Approve All
Exact Matches” for confident links - Manually review and approve/reject
uncertain links - Filter by match type to focus on problematic cases

### Step 6: Create the Links

Once you’ve approved the links, the app writes them to the database.

**What happens:** - Writes to `link_individual_specimen` table with
columns: - `id_n`: Individual ID (foreign key to `data_individuals`) -
`id_specimen`: Specimen ID (foreign key to `specimens`) - `id_linktype`:
Link type ID (e.g., “referenced_individual”, “type_individual”) -
`created_by`: Your username (audit trail) - `created_at`: Timestamp
(audit trail)

**Validation:** - Checks that all foreign key references exist (batch
validation) - Prevents duplicate links - Ensures data integrity

**Result:** - Confirmation message showing number of links created -
Links are now permanent and queryable

## Link Types

The system supports multiple link types to capture the nature of the
relationship:

### `type_individual`

Direct evidence: The herbarium specimen was physically collected **from
this specific tree** during plot census. This provides the highest
confidence link between the measured individual and its taxonomic
identification.

### `referenced_individual`

Indirect evidence: This tree was field-identified as the **same
species** as another tree from which a specimen was collected. The tree
is linked to that specimen, but the specimen itself was not collected
from this individual. This extends the taxonomic value of collected
specimens to additional trees, though with lower confidence since it
relies on field identification accuracy.

**How it’s determined:** - The app checks if `herbarium_nbe_type` column
matches `herbarium_nbe_char` - If yes → `type_individual` (specimen
collected from this tree) - If no → `referenced_individual` (tree
identified as same species)

## Querying Linked Specimens

Once links are established, you can query them:

``` r

library(CafriplotsR)

# Connect to both databases in one step
mydb <- connect_cafri()$main

# Get all specimen links for a plot
links <- query_all_specimen_links(plot_ids = 1, con = mydb)

# Get individuals with their linked specimens
individuals <- query_plots(
  id_plot = 1,
  extract_individuals = TRUE,
  con = mydb
)

# Check which individuals have specimen links
linked_individuals <- individuals %>%
  filter(!is.na(id_specimen))
```

## Benefits in Practice

### Scenario 1: Taxonomic Revision

**Initial state:** - Individual \#12345 identified as *Guarea
thompsonii* in field notes - Specimen DAU-1234 collected from this
individual - Link created between individual and specimen

**Years later:** - Taxonomist revises specimen DAU-1234 → *Guarea
cedrata* - Update made in herbarium database

**Result:** - Next query of individual \#12345 automatically returns
*Guarea cedrata* - No manual updates needed in inventory database -
Historical record preserved: you can still see the original field ID

### Scenario 2: Data Quality Metrics

With specimen links, you can calculate:

``` r

# Proportion of individuals with specimen backing
specimen_coverage <- sum(!is.na(individuals$id_specimen)) / nrow(individuals)

# Taxonomic accuracy: how many field IDs match specimen IDs?
accuracy <- sum(individuals$field_idtax == individuals$specimen_idtax, na.rm = TRUE) /
            sum(!is.na(individuals$id_specimen))
```

### Scenario 3: Citation and Traceability

When publishing, you can now cite the herbarium specimens directly:

> “Species identifications were verified by herbarium specimens (Dauby
> 123, 145, 167; deposited at LBV) collected from the same individuals
> during plot census.”

## Database Schema

The linking system uses these key tables:

    data_individuals
    ├─ id_n (PK)
    ├─ herbarium_nbe_char (specimen reference text)
    ├─ herbarium_nbe_type (reference for specimen collected from this tree)
    └─ idtax_n (field identification)

    specimens
    ├─ id_specimen (PK)
    ├─ id_colnam (collector ID → table_colnam)
    ├─ colnbr (specimen number)
    └─ idtax_specimen (specimen identification)

    link_individual_specimen
    ├─ id_link (PK)
    ├─ id_n (FK → data_individuals)
    ├─ id_specimen (FK → specimens)
    ├─ id_linktype (FK → linktypelist)
    ├─ created_by (audit)
    └─ created_at (audit)

## Troubleshooting

### “No specimens found to retrieve”

**Cause:** The `herbarium_nbe_char` column is empty or unparseable.

**Solution:** - Check that your data has specimen references in the
correct format - Use format: “Collector Name \####” (e.g., “Dauby 123”)

### “Validation failed: Missing specimen IDs”

**Cause:** The specimens don’t exist in the herbarium database yet.

**Solution:** - Ensure specimens are entered in the `specimens` table
first - Check collector name spelling matches `table_colnam`

### “Taxonomic match: Different family”

**Cause:** Field identification and specimen identification are very
different.

**Solution:** - This is often a real discrepancy worth investigating -
Check field notes and specimen label - Consult with taxonomists before
creating the link

## Best Practices

1.  **Link specimens as soon as they’re databased**: Don’t wait years to
    establish connections.

2.  **Review taxonomic discrepancies carefully**: Different families
    often indicate either:

    - Field misidentification (common)
    - Wrong specimen linked (data entry error)
    - Mislabeled specimen (rare)

3.  **Document link types accurately**: Distinguish between direct
    collection (`type_individual`) and field-identified references
    (`referenced_individual`).

4.  **Maintain audit trails**: Never delete links; add new ones if
    corrections are needed.

5.  **Regular updates**: Periodically query for new specimen
    identifications to ensure your inventory data stays current.

## Summary

The specimen linking system transforms static inventory data into a
**living dataset** that improves over time as taxonomic knowledge
advances. By creating formal database-level connections between
ecological measurements and herbarium vouchers, you ensure that your
research benefits from ongoing taxonomic revision—automatically,
permanently, and transparently.

This is not just data management; it’s a paradigm shift toward
**cumulative, self-improving ecological data infrastructure**.
