namespace PetriTool
{
    /// <summary>
    /// Enumerates all the output formats supported by the tool. 
    /// For example, translates nets or generated reachability instances can
    /// be written in any of these formats.
    /// </summary>
    public enum OutputFormat
    {
        Lola,
        TTS,
        Dotspec,
        PNML,
        CGraph,
        TPN
    }
}