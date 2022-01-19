# Test EVT file description

```
tests/testcruise_evt/2014_185/2014-07-04T00-00-02+00-00     # normal file
tests/testcruise_evt/2014_185/2014-07-04T00-03-02+00-00.gz  # normal file, gz
tests/testcruise_evt/2014_185/2014-07-04T00-06-02+00-00     # empty file
tests/testcruise_evt/2014_185/2014-07-04T00-09-02+00-00     # header reports 0 particles
tests/testcruise_evt/2014_185/2014-07-04T00-12-02+00-00     # file only 2 bytes, should be at least 4 for header
tests/testcruise_evt/2014_185/2014-07-04T01-15-02+00-00.gz  # all noise
tests/testcruise_evt/2014_185/2014-07-04T01-17-02+00-00.gz  # only 2 quantiles have OPP
tests/testcruise_evt/2014_185/2014-07-04T01-21-02+00-00     # not in SFL and more data than header reports
tests/testcruise_evt/2014_185/2014-07-04T01-27-02+00-00     # not in SFL and less data than header reports
tests/testcruise_evt/2014_185/2014-07-04T01-30-02+00-00  # normal file, identical to first
```
