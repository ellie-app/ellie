import os
import random
import time
from math import floor
from typing import (Any, Dict, Iterator, List, NamedTuple, Optional,
                    SupportsInt, TypeVar)

T = TypeVar('T')


def cat_optionals(data: Iterator[Optional[T]]) -> List[T]:
    out = []
    for x in data:
        if x is not None:
            out.append(x)
    return out


class ApiError(Exception):
    def __init__(self, status_code: int, message: str) -> None:
        self.status_code = status_code
        self.message = message


class Version(SupportsInt):
    def __init__(self, major: int, minor: int, patch: int) -> None:
        self.major = major
        self.minor = minor
        self.patch = patch

    def __hash__(self) -> int:
        return hash(self.__int__())

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, Version):
            return NotImplemented
        return self.__int__() < int(other)

    def __le__(self, other: object) -> bool:
        if not isinstance(other, Version):
            return NotImplemented
        return self.__int__() <= int(other)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Version):
            return NotImplemented
        return self.__int__() == int(other)

    def __int__(self) -> int:
        return (self.major << 20) | (self.minor << 10) | self.patch

    def __str__(self) -> str:
        return str(self.major) + '.' + str(self.minor) + '.' + str(self.patch)

    def __repr__(self) -> str:
        return '<Version ' + self.__str__() + '>'

    def next_major(self) -> 'Version':
        return Version(self.major + 1, 0, 0)

    def next_patch(self) -> 'Version':
        return Version(self.major, self.minor, self.patch + 1)

    def to_json(self) -> object:
        return self.__str__()

    @staticmethod
    def from_int(value: int) -> 'Version':
        return Version(value >> 20, (value >> 10) & 0b1111111111,
                       value & 0b1111111111)

    @staticmethod
    def from_string(input: str) -> Optional['Version']:
        try:
            split = input.split('.')
            as_ints = list(map(int, split))
            all_worked = all(isinstance(x, int) for x in as_ints)
            if all_worked and len(as_ints) == 3:
                return Version(as_ints[0], as_ints[1], as_ints[2])
            return None
        except:
            return None

    @staticmethod
    def from_json(data: Any) -> Optional['Version']:
        return Version.from_string(data)


class Constraint(object):
    def __init__(self,
                 lower: Version,
                 lower_op: str,
                 upper_op: str,
                 upper: Version) -> None:
        self.lower = lower
        self.lower_op = lower_op
        self.upper_op = upper_op
        self.upper = upper

    def __str__(self) -> str:
        return str(self.lower
                   ) + ' ' + self.lower_op + ' v ' + self.upper_op + ' ' + str(
                       self.upper)

    def __repr__(self) -> str:
        return '<Constraint ' + self.__str__() + '>'

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Constraint):
            return False
        return self.min_version() == other.min_version() and self.max_version(
        ) == other.max_version()

    def is_satisfied(self, version: Version) -> bool:
        return self.min_version() <= version < self.max_version()

    def min_version(self) -> Version:
        if self.lower_op == '<':
            return self.lower.next_patch()
        return self.lower

    def max_version(self) -> Version:
        if self.upper_op == '<':
            return self.upper
        return self.upper.next_patch()

    @staticmethod
    def from_ints(left: int, right: int) -> 'Constraint':
        return Constraint(
            Version.from_int(left), '<=', '<', Version.from_int(right))

    @staticmethod
    def from_string(input: str) -> Optional['Constraint']:
        split = input.split('v')
        trimmed = list(map(lambda x: x.strip(' '), split))
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
    def from_json(input: object) -> Optional['Constraint']:
        if not isinstance(input, str):
            return None
        return Constraint.from_string(input)

    def to_json(self) -> object:
        return self.__str__()


class PackageName(object):
    def __init__(self, user: str, project: str) -> None:
        self.user = user
        self.project = project

    def __str__(self) -> str:
        return self.user + '/' + self.project

    def __repr__(self) -> str:
        return '<PackageName ' + self.__str__() + '>'

    def __hash__(self) -> int:
        return hash(self.__str__())

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, PackageName):
            return NotImplemented
        return self.user == other.user and self.project == other.project

    def to_json(self) -> object:
        return self.__str__()

    @staticmethod
    def from_json(data: Any) -> Optional['PackageName']:
        split = data.split('/')
        if len(split) != 2:
            return None

        return PackageName(split[0], split[1])


class Package(object):
    def __init__(self, name: 'PackageName', version: Version) -> None:
        self.name = name
        self.version = version

    def to_json(self) -> object:
        return [self.name.to_json(), self.version.to_json()]

    @staticmethod
    def from_json(data: Any) -> Optional['Package']:
        name = PackageName.from_json(data[0])
        if name is None:
            return None

        version = Version.from_json(data[1])
        if version is None:
            return None

        return Package(name, version)


class PackageInfo(object):
    def __init__(self, username: str, package: str, version: Version) -> None:
        self.username = username
        self.package = package
        self.version = version
        self.elm_constraint: Optional[Constraint] = None

    def __str__(self) -> str:
        return self.username + '/' + self.package + '@' + str(self.version)

    def __repr__(self) -> str:
        return '<PackageInfo ' + self.__str__() + '>'

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, PackageInfo):
            return False
        return self.username == other.username and \
            self.package == other.package and \
            self.version == other.version

    def __neq__(self, other: object) -> bool:
        return not self.__eq__(other)

    def __hash__(self) -> int:
        return hash(self.__str__())

    def s3_package_key(self) -> str:
        return 'package-artifacts/' + self.username + '/' + self.package + '/' + str(self.version) + '/elm-package.json'

    def s3_source_key(self) -> str:
        return 'package-artifacts/' + self.username + '/' + self.package + '/' + str(self.version) + '/source.json'

    def s3_artifacts_key(self, version: Version) -> str:
        return 'package-artifacts/' + self.username + '/' + self.package + '/' + str(self.version) + '/artifacts/' + str(version) + '.json'

    def set_elm_constraint(self, constraint: Optional[Constraint]) -> None:
        self.elm_constraint = constraint

    def to_json(self) -> object:
        return {
            'username': self.username,
            'package': self.package,
            'version': self.version.to_json(),
            'elmVersion': self.elm_constraint.to_json() if self.elm_constraint is not None else None
        }

    def to_package(self) -> Package:
        return Package(PackageName(self.username, self.package), self.version)

    @staticmethod
    def from_json(data: Dict[str, Any]) -> Optional['PackageInfo']:
        version = Version.from_string(data['version'])
        if version is None:
            return None

        package = PackageInfo(data['username'], data['package'], version)
        if 'minElmVersion' in data and 'maxElmVersion' in data:
            package.set_elm_constraint(
                Constraint.from_ints(data['minElmVersion'], data[
                    'maxElmVersion']))
        elif 'elmVersion' in data:
            package.set_elm_constraint(
                Constraint.from_json(data['elmVersion']))
        return package


def timestamp() -> int:
    return int(time.time() * 1000)


ALPHABET = '23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ'
BASE_LENGTH = len(ALPHABET)
SEQ_ID = 0
RELEASE_ID = int(os.environ['HEROKU_RELEASE_VERSION'].lstrip('v'))
OUR_EPOCH = timestamp()


class ProjectId(SupportsInt):
    def __init__(self, number_value: int, version: int) -> None:
        self._number_value = number_value
        self.is_old = version != 1

    def __hash__(self) -> int:
        return hash(self.__int__())

    def __eq__(self, other: object) -> bool:
        if isinstance(other, ProjectId):
            return self.__int__() == other.__int__()
        else:
            return False

    def __ne__(self, other: object) -> bool:
        return (not self.__eq__(other))

    def __str__(self) -> str:
        return self._to_string_v1(self._number_value)

    def __repr__(self) -> str:
        return '<ProjectId ' + self.__str__() + '>'

    def __int__(self) -> int:
        return self._number_value

    def _to_string_v1(self, number_value: int) -> str:
        tracker = int(number_value)
        output = ''
        while tracker > 0:
            index = tracker % BASE_LENGTH
            output = ALPHABET[index] + output
            tracker = tracker // BASE_LENGTH
        return output + 'a1'

    def _to_string_v0(self, number_value: int) -> str:
        tracker = int(number_value)
        output = ''
        while tracker > 0:
            index = (tracker % len(ALPHABET)) - 1
            if index >= 0:
                output = ALPHABET[index] + output
            tracker = tracker // BASE_LENGTH
        return output

    def to_json(self) -> object:
        return self.__str__()

    @staticmethod
    def generate() -> 'ProjectId':
        global SEQ_ID
        my_seq_id = (SEQ_ID + 1) % 1024
        SEQ_ID += 1
        now_millis = timestamp()
        result = (now_millis - OUR_EPOCH) << 23
        result |= RELEASE_ID << 10
        result |= my_seq_id
        return ProjectId(result, 1)

    @staticmethod
    def _from_string_v0(input: str) -> 'ProjectId':
        tracker = 0
        length = len(input)
        for i in range(length):
            index = ALPHABET.index(input[i]) + 1
            tracker = tracker * BASE_LENGTH + index
        return ProjectId(tracker, 0)

    @staticmethod
    def _from_string_v1(input: str) -> 'ProjectId':
        tracker = 0
        without_id = input.replace('a1', '')
        length = len(without_id)
        for i in range(length):
            index = ALPHABET.index(without_id[i])
            tracker = tracker * BASE_LENGTH + index
        return ProjectId(tracker, 1)

    @staticmethod
    def _determine_version(input: str) -> int:
        if input.endswith('a1'):
            return 1
        return 0

    @staticmethod
    def from_string(input: str) -> Optional['ProjectId']:
        if input.isdigit():
            return ProjectId(int(input), 1)

        version = ProjectId._determine_version(input)
        if version == 1:
            return ProjectId._from_string_v1(input)
        elif version == 0:
            return ProjectId._from_string_v0(input)
        else:
            return None

    @staticmethod
    def from_json(input: Any) -> Optional['ProjectId']:
        if isinstance(input, str):
            return ProjectId.from_string(input)
        return None


class RevisionId(object):
    def __init__(self, project_id: ProjectId, revision_number: int) -> None:
        self.project_id = project_id
        self.revision_number = revision_number

    def to_json(self) -> Any:
        return {
            'projectId': self.project_id.to_json(),
            'revisionNumber': self.revision_number
        }

    @staticmethod
    def from_json(data: Any) -> Optional['RevisionId']:
        project_id = ProjectId.from_json(data['projectId'])
        if project_id is None:
            return None
        return RevisionId(project_id, data['revisionNumber'])


class RevisionBase(NamedTuple):
    title: str
    description: str
    elm_code: str
    html_code: str
    packages: List[Package]
    id: Optional[RevisionId]
    owned: bool
    snapshot: Any
    elm_version: Version


class Revision(RevisionBase):
    def to_json(self) -> Any:
        return {
            'title': self.title,
            'description': self.description,
            'elmCode': self.elm_code,
            'htmlCode': self.html_code,
            'packages': [p.to_json() for p in self.packages],
            'id': self.id.to_json() if self.id is not None else None,
            'owned': self.owned,
            'snapshot': self.snapshot,
            'elmVersion': self.elm_version.to_json()
        }

    @staticmethod
    def from_json(data: Any) -> Optional['Revision']:
        elm_version = Version(0, 18, 0)
        if 'elmVersion' in data:
            parsed = Version.from_json(data['elmVersion'])
            if parsed is not None:
                elm_version = parsed

        return Revision(
            title=data['title'],
            description=data['description'],
            elm_code=data['elmCode'],
            html_code=data['htmlCode'],
            packages=cat_optionals(
                Package.from_json(x) for x in data['packages']),
            id=RevisionId.from_json(data['id']),
            owned=data['owned'] if 'owned' in data else False,
            snapshot=data['snapshot'],
            elm_version=elm_version)
