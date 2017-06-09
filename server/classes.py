from math import floor
import time

class ApiError(Exception):
    def __init__(self, status_code):
        self.status_code = status_code


class Version(object):
    def __init__(self, major, minor, patch):
        self.major = major
        self.minor = minor
        self.patch = patch

    def __hash__(self):
        return self.__int__()

    def __lt__(self, other):
        return self.__int__() < int(other)

    def __le__(self, other):
        return self.__int__() <= int(other)

    def __eq__(self, other):
        if not isinstance(other, Version):
            return NotImplemented
        return self.__int__() == int(other)

    def __int__(self):
        return (self.major << 20) | (self.minor << 10) | self.patch

    def __str__(self):
        return str(self.major) + '.' + str(self.minor) + '.' + str(self.patch)

    def __repr__(self):
        return '<Version ' + self.__str__() + '>'

    def next_major(self):
        return Version(self.major + 1, 0, 0)

    def next_patch(self):
        return Version(self.major, self.minor, self.patch + 1)

    def to_json(self):
        return self.__str__()

    @staticmethod
    def from_int(value):
        return Version(
            value >> 20,
            (value >> 10) & 0b1111111111,
            value & 0b1111111111
        )

    @staticmethod
    def from_string(input):
        try:
            split = input.split('.')
            as_ints = map(lambda x: int(x), split)
            all_worked = all([isinstance(x, int) for x in as_ints])
            if all_worked:
                return Version(as_ints[0], as_ints[1], as_ints[2])
            return None
        except:
            return None


class Constraint(object):
    def __init__(self, lower, lower_op, upper_op, upper):
        self.lower = lower
        self.lower_op = lower_op
        self.upper_op = upper_op
        self.upper = upper

    def __str__(self):
        return str(self.lower) + ' ' + self.lower_op + ' v ' + self.upper_op + ' ' + str(self.upper)

    def __repr__(self):
        return '<Constraint ' + self.__str__() + '>'

    def __eq__(self, other):
        return self.min_version() == other.min_version() and self.max_version() == other.max_version()

    def is_satisfied(self, version):
        return self.min_version() <= version < self.max_version()

    def min_version(self):
        if self.lower_op == '<':
            return self.lower.next_patch()
        return self.lower

    def max_version(self):
        if self.upper_op == '<':
            return self.upper
        return self.upper.next_patch()

    @staticmethod
    def from_ints(left, right):
        return Constraint(
            Version.from_int(left),
            '<=',
            '<',
            Version.from_int(right)
        )

    @staticmethod
    def from_string(input):
        split = input.split('v')
        trimmed = map(lambda x: x.strip(' '), split)
        left_stuff = trimmed[0]
        right_stuff = trimmed[1]

        if left_stuff is None or right_stuff is None:
            return None

        left_op = '<=' if left_stuff.endswith('<=') else '<'
        left_version = Version.from_string(left_stuff.rstrip(left_op + ' '))
        right_op = '<=' if right_stuff.startswith('<=') else '<'
        right_version = Version.from_string(right_stuff.lstrip(right_op + ' '))

        if left_version is None or right_version is None:
            return None

        return Constraint(left_version, left_op, right_op, right_version)

    @staticmethod
    def from_json(input):
        return Constraint.from_string(input)

    def to_json(self):
        return self.__str__()

class PackageName(object):
    def __init__(self, user, project):
        self.user = user
        self.project = project

    def __str__(self):
        return self.user + '/' + self.project

    def __repr__(self):
        return '<PackageName ' + self.__str__() + '>'

    def __hash__(self):
        return hash(self.__str__())

    def __eq__(self, other):
        if not isinstance(other, PackageName):
            return NotImplemented
        return self.user == other.user and self.project == other.project

    def to_json(self):
        return self.__str__()


class Package(object):
    def __init__(self, name, version):
        self.name = name
        self.version = version

    def to_json(self):
        return [self.name.to_json(), self.version.to_json()]


class PackageInfo(object):
    def __init__(self, username, package, version):
        self.username = username
        self.package = package
        self.version = version
        self.elm_constraint = None

    def __str__(self):
        return self.username + '/' + self.package + '@' + str(self.version)

    def __repr__(self):
        return '<PackageInfo ' + self.__str__() + '>'

    def __eq__(self, other):
        if not isinstance(other, PackageInfo):
            return False
        return self.username == other.username and self.package == other.package and self.version == other.version and self.elm_constraint == other.elm_constraint

    def __neq__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.__str__())

    def s3_package_key(self):
        return 'package-artifacts/package-' + self.username + '-' + self.package + '-' + str(self.version) + '.json'

    def s3_source_key(self):
        return 'package-artifacts/source-' + self.username + '-' + self.package + '-' + str(self.version) + '.json'

    def set_elm_constraint(self, constraint):
        self.elm_constraint = constraint

    def to_json(self):
        return {
            'username': self.username,
            'package': self.package,
            'version': self.version.to_json(),
            'elmVersion': self.elm_constraint.to_json()
        }

    def to_package(self):
        return Package(
            PackageName(self.username, self.package),
            self.version
        )

    @staticmethod
    def from_json(data):
        version = Version.from_string(data['version'])
        if version is None:
            return None

        package = PackageInfo(data['username'], data['package'], version)
        if 'minElmVersion' in data and 'maxElmVersion' in data:
            package.set_elm_constraint(Constraint.from_ints(data['minElmVersion'], data['maxElmVersion']))
        elif 'elmVersion' in data:
            package.set_elm_constraint(Constraint.from_json(data['elmVersion']))
        return package


ALPHABET = '23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ'
BASE_LENGTH = len(ALPHABET)
OUR_EPOCH = 1483578344834L
SHARD_ID = 1L
SEQ_ID = 0L

def timestamp():
    return long(time.time() * 1000)

class ProjectId(object):
    def __init__(self, number_value, version):
        self._number_value = number_value
        self.is_old = version != 1

    def __hash__(self):
        return hash(self.__int__())

    def __eq__(self, other):
        if isinstance(other, ProjectId):
            return self.__int__() == other.__int__()
        else:
            return False

    def __ne__(self, other):
        return (not self.__eq__(other))

    def __str__(self):
        return self._to_string_v1(self._number_value)

    def __repr__(self):
        return '<ProjectId ' + self.__str__() + '>'

    def __int__(self):
        return self._number_value

    def _to_string_v1(self, number_value):
        tracker = long(number_value)
        output = ''
        while tracker > 0:
            output = ALPHABET[tracker % BASE_LENGTH] + output
            tracker = long(floor(float(tracker) / BASE_LENGTH))
        return output + 'a1'

    def _to_string_v0(self, number_value):
        tracker = long(number_value)
        output = ''
        while tracker > 0:
            index = (tracker % ALPHABET) - 1
            if index >= 0:
                output = ALPHABET[index] + output
            tracker = long(floor(float(tracker) / BASE_LENGTH))
        return output

    @staticmethod
    def generate():
        global SEQ_ID
        my_seq_id = (SEQ_ID + 1) % 1024
        SEQ_ID += 1
        now_millis = timestamp()
        result = (now_millis - OUR_EPOCH) << 23
        result = result | (SHARD_ID << 10)
        result = result | my_seq_id
        return ProjectId(result, 1)

    @staticmethod
    def _from_string_v0(input):
        tracker = 0L
        length = len(input)
        for i in range(length):
            index = long(ALPHABET.index(input[i])) + 1L
            tracker = tracker * long(BASE_LENGTH) + index
        return ProjectId(tracker, 0)

    @staticmethod
    def _from_string_v1(input):
        tracker = 0L
        without_id = input.replace('a1', '')
        length = len(without_id)
        for i in range(length):
            index = long(ALPHABET.index(without_id[i]))
            tracker = tracker * long(BASE_LENGTH) + index
        return ProjectId(tracker, 1)

    @staticmethod
    def _determine_version(input):
        if input.endswith('a1'):
            return 1
        return 0

    @staticmethod
    def from_string(input):
        if input.isdigit():
            return ProjectId(long(input), 1)

        version = ProjectId._determine_version(input)
        if version == 1:
            return ProjectId._from_string_v1(input)
        elif version == 0:
            return ProjectId._from_string_v0(input)
        else:
            return None
