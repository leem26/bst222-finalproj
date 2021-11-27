# BST 222 (Fall 2021) Group Project

NHANES data from 1999-2018 are available as SAS files [online](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017), along with codebooks and documentation.

**File tree:**

```bash
.
├── R
├── README.md
├── data
│ └── raw-nhanes
│     ├── BMX_J.xpt
│     ├── BPQ_J.xpt
│     ├── BPX_J.xpt
│     ├── DEMO_J.xpt
│     ├── DXXAG_J.xpt
│     └── WHQ_J.xpt
├── out
└── refs
    └── nihms-1590171.pdf
```

**Description of data files**

Note: *_J* refers to the 2017-2018 cycle of NHANES

- "BMX": Body measures exam, including measured height and weight
- "BPQ": Blood pressure and cholesterol self-report questionnaire
- "BPX": Blood pressure measured exam
- "DEMO": Demographics files
- "DXXAG": DEXA (gold standard fat mass measure) measures
- "WHQ": Weight history questionnaire, includes self-reported height and weight

**Running Analyses**

1. 