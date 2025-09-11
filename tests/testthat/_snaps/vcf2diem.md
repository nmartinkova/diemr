# incorrect chunk size

    Code
      vcf2diem(SNP = filename, filename = "test.txt", chunk = 3)
    Message
      Expecting to include 8 markers per diem file.
      If you expect more markers in the file, provide a suitable chunk size.
      Done with chunk 1

# resolve multiallelic markers

    Code
      vcf2diem(SNP = brenthis, filename = "brenthis.txt", chunk = 1)
    Message
      Expecting to include 29 markers per diem file.
      If you expect more markers in the file, provide a suitable chunk size.

---

    Code
      vcf2diem(SNP = myotis, filename = "myotis.txt", chunk = 1)
    Message
      Expecting to include 22 markers per diem file.
      If you expect more markers in the file, provide a suitable chunk size.

# higher number of homozygotes

    NULL

---

    NULL

