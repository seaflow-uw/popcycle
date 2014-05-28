#!/usr/bin/env python
import os
import struct


def load_evt(filename, cruise=None, file=None):
    """Load an EVT or OPP file in the EVT.HEADER.V3 format as specified by
    https://github.com/fribalet/flowPhyto/blob/master/R/Globals.R"""

    # The EVTv3 schema
    EVT_V3_SCHEMA = ["time", "pulse_width", "D1", "D2", "fsc_small", "fsc_perp",
                     "fsc_big", "pe", "chl_small", "chl_big"]
    # There are 10 fields in the EVTv3 schema
    NUM_FIELDS = len(EVT_V3_SCHEMA)
    # Each field is 2 bytes, plus there's a 4-byte separator
    EVT_ROW_SIZE = NUM_FIELDS * 2 + 4
    # The magic number in the EOL character
    EOL_MAGIC = 10

    with open(filename, 'rb') as file_:
        # Header -  LE 4-byte bytes telling us the number of particles
        num_particles = struct.unpack_from('<I', file_.read(4))[0]
        expected_size = num_particles * EVT_ROW_SIZE + 4
        file_size = os.path.getsize(filename)
        if file_size != expected_size:
            raise IOError(
                "With {} rows, file size of {} should be {} not {}".format(
                    num_particles, filename, expected_size, file_size))

        # The pattern for a record is one unsigned short per field plus
        # 4-byte EOL. However, the file format actually makes it easier to
        # treat the EOL character as coming at the beginning of the line
        # (it's dropped from the last line).
        record_pattern = '<' + 'I' + ('H' * NUM_FIELDS)
        record_size = struct.calcsize(record_pattern)
        ret = []
        for i in range(num_particles):
            data = file_.read(record_size)
            record = struct.unpack_from(record_pattern, data)
            if record[0] != EOL_MAGIC:
                raise IOError(
                    "particle {} has incorrect EOL value {} instead of {}"
                    .format(i, record[0], EOL_MAGIC))
            particle = {field: value
                         for (field, value)
                         in zip(EVT_V3_SCHEMA, record)}
            particle.update(cruise=cruise, file=file, particle=i)
            ret.append(particle)

        return ret


if __name__ == "__main__":
    import sys

    particles = load_evt(sys.argv[1])
    print("Loaded {} particles from {}".format(len(particles), sys.argv[1]))