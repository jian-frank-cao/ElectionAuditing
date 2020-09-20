# Election Auditing
## Voter Registration Database Auditing
1. Read the VR snapshot: [fread_file_latin1.R](./fread_file_latin1.R)
2. Clean data set: [data_clean_v3.R](./data_clean_v3.R)
3. Data quality validation: [data_validation_v3.R](./data_validation_v3.R)
4. Detect changes in snapshots: [changes_in_records.R](./changes_in_records.R)
5. Detect exactly matched duplicates: [find_dups_exact_v3.R](./find_dups_exact_v3.R)
6. Split batches to reduce computing burden: [split_subsets_v3.R](./split_subsets_v3.R)
7. Detect probabilistically matched duplicates: [find_dups_pbl_v9.R](./find_dups_pbl_v9.R)
