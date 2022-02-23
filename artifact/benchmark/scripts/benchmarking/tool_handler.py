
from abc import ABC, abstractmethod
import benchmark_utils as utils


class AbstractToolHandler(ABC):

    @abstractmethod
    def get_tool_name(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def get_net_extensions(self) -> list:
        """
        Returns a list of compatible extensions for net files that the tool can handle.
        Extensions with higher preference should be earlier in the list.
        For a given benchmark file, only the file with the first present extension will be consumed.
        """
        raise NotImplementedError

    @abstractmethod
    def get_target_extensions(self) -> list:
        """
        Returns a list of compatible extensions for target files that the tool can handle.
        Extensions with higher preference should be earlier in the list.
        For a given benchmark file, only the file with the first present extension will be consumed.
        """
        raise NotImplementedError

    @abstractmethod
    def run(self, net_file, target_file, timeout) -> dict:
        """
        Runs this tool on the given net and target file.
        Returns a dict object as the result with some execution information,
        which can contain different amounts of information depending on the tool.
        Will abort after timeout time.
        """
        raise NotImplementedError


class LolaHandler(AbstractToolHandler):

    def get_tool_name(self):
        return "LoLA"

    def get_net_extensions(self):
        return [".lola"]

    def get_target_extensions(self):
        return [".formula"]

    def run(self, net_file, target_file, timeout):
        return utils.call_lola(net_file, target_file, timeout)


class FastForwardHandler(AbstractToolHandler):

    def __init__(self, method_name, args, do_pruning):
        self.method_name = method_name
        self.args = args
        self.do_pruning = do_pruning

    def get_tool_name(self):
        return "FastForward_" + self.method_name + self.args + ("+Pruning" if self.do_pruning else "")

    def get_net_extensions(self):
        return [".lola", ".spec", ".tts"]

    def get_target_extensions(self):
        return [".formula", ".spec", ".prop"]

    def run(self, net_file, target_file, timeout_time):
        return utils.call_fastforward(self.method_name, net_file, target_file, self.do_pruning, self.args, timeout_time)


class BFCHandler(AbstractToolHandler):

    def get_tool_name(self):
        return "BFC"

    def get_net_extensions(self):
        return [".tts", ".spec.tts"]

    def get_target_extensions(self):
        return [".prop", ".tts.prop", ".spec.tts.prop"]

    def run(self, net_file, target_file, timeout):
        return utils.call_bfc("bfc/bfc", net_file, target_file, timeout)


class BFCOldHandler(AbstractToolHandler):

    def get_tool_name(self):
        return "BFCOld"

    def get_net_extensions(self):
        return [".tts"]

    def get_target_extensions(self):
        return [".prop"]

    def run(self, net_file, target_file, timeout):
        return utils.call_bfc("bfcold", net_file, target_file, timeout)


class ICoverHandler(AbstractToolHandler):

    def get_tool_name(self):
        return "ICover"

    def get_net_extensions(self):
        return [".spec"]

    def get_target_extensions(self):
        return [".spec"]

    def run(self, net_file, target_file, timeout):
        return utils.call_icover(net_file, timeout)

class WoflanHandler(AbstractToolHandler):

    def get_tool_name(self):
        return "Woflan"

    def get_net_extensions(self):
        return [".pnml"]

    def get_target_extensions(self):
        return []

    def run(self, net_file, target_file, timeout):
        return utils.call_woflan(net_file, timeout)


class MISTHandler(AbstractToolHandler):
    def get_tool_name(self):
        return "MIST"

    def get_net_extensions(self):
        return [".spec"]

    def get_target_extensions(self):
        return [".spec"]

    def run(self, net_file, target_file, timeout):
        return utils.call_mist(net_file, timeout)


class KosarajuHandler(AbstractToolHandler):

    def __init__(self, is_coverability):
        self.is_coverability = is_coverability

    def get_tool_name(self):
        return "Kosaraju"

    def get_net_extensions(self):
        return [".spec"]

    def get_target_extensions(self):
        return [".spec"]

    def run(self, net_file, target_file, timeout):
        return utils.call_kosaraju(net_file, timeout, self.is_coverability)
