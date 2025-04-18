# Transformed Package (R)

## Overview

This package reproduces selected tables and figures from the associated paper. For full reproducibility of all results, please refer to the original Replication Package.

## Instructions

1. **Download all folders and files** and keep the folder structure intact.
2. Navigate to the `R/` folder:
   - You'll find three scripts:
     - `master.r`
     - `01_maketables.R`
     - `02_makegraphs.R`
3. In `master.r`, **change the path name on line 8** to match your local file system.
4. Run `master.r` to execute the full workflow.

## Contents

### Tables

| Table | Source Script     | Line(s)                        | Output Type      | Notes                                                                 |
|-------|-------------------|--------------------------------|------------------|-----------------------------------------------------------------------|
| 1     | `01_maketables.R` | Line 52                        | TeX file         |                                                                       |
| 2     | `01_maketables.R` | Line 96                        | TeX file         |                                                                       |
| 3     | `01_maketables.R` | Lines 139, 172, 202            | Printed results  | Code for the first column (line 139) is suppressed for performance.  |

### Figures

| Figure | Source Script     | Line(s)                        | Output Type | Notes                     |
|--------|-------------------|--------------------------------|-------------|---------------------------|
| 1      | `02_makegraphs.R` | Line 34                        | PDF         |                           |
| 2      | `02_makegraphs.R` | Line 87                        | PDF         |                           |
| 3      | `02_makegraphs.R` | Line 141                       | PDF         |                           |
| 4      | `02_makegraphs.R` | Line 199                       | PDF         |                           |
| 5      | `02_makegraphs.R` | Lines 212, 221, 230, 239       | PDF         | Multiple related outputs  |
| 6      | `02_makegraphs.R` | Lines 280, 289, 312            | PDF         | Multiple related outputs  |
| 7      | `02_makegraphs.R` | Line 350                       | PDF         |                           |

## Notes

- Some code (e.g., for Table 3, Column 1) is intentionally suppressed to speed up first runs.
- Be sure to check the output folder structure and naming conventions for all generated files.
