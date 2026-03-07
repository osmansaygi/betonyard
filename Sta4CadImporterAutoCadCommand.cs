using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.Colors;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.Windows;

namespace BetonLispAutoCad
{
    public class Sta4CadImporterAutoCadCommand
    {
        private const double ColumnRotationOffsetDeg = -90.0;
        private const int BeamEccSignMultiplier = -1;
        private const bool SwapColumnSectionAxes = true;
        private static readonly bool UseColumnAxisEccentricity = true;

        [CommandMethod("ImportSTA4CAD")]
        public void ImportSTA4CAD()
        {
            var doc = Application.DocumentManager.MdiActiveDocument;
            if (doc == null)
                return;

            var db = doc.Database;
            var ed = doc.Editor;

            var fileDialog = new OpenFileDialog(
                "STA4CAD Projesi Sec",
                string.Empty,
                "st4",
                "STA4CAD_ST4",
                OpenFileDialog.OpenFileDialogFlags.DefaultIsFolder);

            if (fileDialog.ShowDialog() != System.Windows.Forms.DialogResult.OK)
                return;

            var filePath = fileDialog.Filename;
            if (string.IsNullOrWhiteSpace(filePath) || !File.Exists(filePath))
            {
                ed.WriteMessage("\nGecerli bir .st4 dosyasi secilemedi.");
                return;
            }

            string[] lines;
            try
            {
                lines = File.ReadAllLines(filePath);
            }
            catch (System.Exception ex)
            {
                ed.WriteMessage($"\nDosya okunamadi: {ex.Message}");
                return;
            }

            var content = string.Join(Environment.NewLine, lines);
            St4Model model;
            try
            {
                if (!St4ModelBuilder.TryBuild(lines, content, out model, out var buildError))
                {
                    if (!string.IsNullOrWhiteSpace(buildError))
                        Application.ShowAlertDialog(buildError);
                    return;
                }

                CadRenderer.Render(doc, db, ed, model);
            }
            catch (System.Exception ex)
            {
                Application.ShowAlertDialog($"Iceri aktarma hatasi: {ex.Message}");
                return;
            }

            ed.WriteMessage("\nDinamik Tarama Basarili!");
            ed.WriteMessage($"\nCizilen Kolon: {model.Columns.Count}");
            ed.WriteMessage($"\nCizilen Kiris: {model.Beams.Count}");
            ed.WriteMessage($"\nCizilen Doseme: {model.Slabs.Count}");
            ed.Command("_.ZOOM", "_E");
        }

        private sealed class St4Model
        {
            public St4Model(
                double zTop,
                Dictionary<int, double> storyTopByNo,
                Dictionary<int, double> yCoords,
                Dictionary<int, double> xBase,
                Dictionary<(int xId, int yId), Point2d> nodes,
                List<ColumnData> columns,
                List<BeamData> beams,
                List<SlabData> slabs)
            {
                ZTop = zTop;
                StoryTopByNo = storyTopByNo;
                YCoords = yCoords;
                XBase = xBase;
                Nodes = nodes;
                Columns = columns;
                Beams = beams;
                Slabs = slabs;
            }

            public double ZTop { get; }
            public Dictionary<int, double> StoryTopByNo { get; }
            public Dictionary<int, double> YCoords { get; }
            public Dictionary<int, double> XBase { get; }
            public Dictionary<(int xId, int yId), Point2d> Nodes { get; }
            public List<ColumnData> Columns { get; }
            public List<BeamData> Beams { get; }
            public List<SlabData> Slabs { get; }
        }

        private static class St4ModelBuilder
        {
            public static bool TryBuild(string[] lines, string content, out St4Model model, out string error)
            {
                model = null;
                error = string.Empty;

                var sections = ParseSections(lines);
                var axisData = GetSection(sections, "/Axis data");
                var colAxisData = GetSection(sections, "/Column axis");
                var colLines = GetSection(sections, "/Columns Data");
                var beamLines = GetSection(sections, "/Beams Data");
                var slabLines = GetSection(sections, "/Floors Data");
                var storyData = GetSection(sections, "/Story");

                if (axisData.Count == 0)
                {
                    error = "Dosyada Aks verisi bulunamadi.";
                    return false;
                }

                var storyTops = ParseStoryTops(storyData, 3.0);
                var storyTopByNo = BuildStoryTopByNumber(storyTops);
                var zTop = storyTopByNo.Count > 0 ? storyTopByNo.Values.Max() : 3.0;

                var maxY = 1000;
                var maxX = 2000;
                var yMatches = Regex.Matches(content, @"\b10\d{2}\b");
                if (yMatches.Count > 0)
                    maxY = Math.Max(maxY, yMatches.Cast<Match>().Select(m => ParseInt(m.Value, 1000)).Max());

                var xMatches = Regex.Matches(content, @"\b20\d{2}\b");
                if (xMatches.Count > 0)
                    maxX = Math.Max(maxX, xMatches.Cast<Match>().Select(m => ParseInt(m.Value, 2000)).Max());

                var ny = Math.Max(0, maxY - 1000);
                var nx = Math.Max(0, maxX - 2000);

                var yCoords = new Dictionary<int, double>();
                var xBase = new Dictionary<int, double>();
                var xSkews = new Dictionary<int, double>();

                var validAxisLines = axisData
                    .Where(l => l.Split(',').Length >= 2)
                    .ToList();

                for (var i = 0; i < ny && i < validAxisLines.Count; i++)
                {
                    var p = validAxisLines[i].Split(',');
                    yCoords[1001 + i] = ParseDouble(GetPart(p, 1), 0.0);
                }

                for (var i = 0; i < nx && (ny + i) < validAxisLines.Count; i++)
                {
                    var p = validAxisLines[ny + i].Split(',');
                    xSkews[2001 + i] = ParseDouble(GetPart(p, 0), 0.0);
                    xBase[2001 + i] = ParseDouble(GetPart(p, 1), 0.0);
                }

                var nodes = new Dictionary<(int xId, int yId), Point2d>();
                foreach (var yEntry in yCoords)
                {
                    var yId = yEntry.Key;
                    var y = yEntry.Value;
                    foreach (var xEntry in xBase)
                    {
                        var xId = xEntry.Key;
                        var baseX = xEntry.Value;
                        var skew = xSkews.TryGetValue(xId, out var s) ? s : 0.0;
                        var staY = y;
                        var staX = Math.Abs(skew) > 1e-12 ? baseX + (skew * staY) : baseX;
                        var cadPt = new Point2d(y, staX);
                        cadPt = new Point2d(cadPt.X, -cadPt.Y);
                        nodes[(xId, yId)] = cadPt;
                    }
                }

                var colMapping = new Dictionary<int, (int yId, int xId, double eccX, double eccY, double rot)>();
                for (var i = 0; i < colAxisData.Count; i++)
                {
                    var p = colAxisData[i].Split(',');
                    if (p.Length > 6)
                    {
                        colMapping[i + 1] = (
                            ParseInt(GetPart(p, 1), 0),
                            ParseInt(GetPart(p, 2), 0),
                            ParseDouble(GetPart(p, 3), 0.0),
                            ParseDouble(GetPart(p, 4), 0.0),
                            ParseDouble(GetPart(p, 6), 0.0)
                        );
                    }
                }

                var columns = new List<ColumnData>();
                var colBlocks = GroupElements(colLines);
                for (var i = 0; i < colBlocks.Count; i++)
                {
                    var el = colBlocks[i];
                    if (el.Count == 0)
                        continue;

                    var p1 = el[0];
                    var elementNo = ParseInt(GetPart(p1, 0), 0);
                    var name = "S" + GetPart(p1, 0);
                    var storyNo = Math.Max(1, elementNo / 100);
                    var storyTop = GetStoryTop(storyTopByNo, storyNo, zTop);
                    var storyBase = storyNo > 1
                        ? GetStoryTop(storyTopByNo, storyNo - 1, 0.0)
                        : 0.0;
                    var storyHeightRef = GetStoryHeight(storyTopByNo, storyNo, storyTop > 0.0 ? storyTop : zTop);
                    var b = ParseDouble(GetPart(p1, 1), 0.0) / 100.0;
                    var d = ParseDouble(GetPart(p1, 2), 0.0) / 100.0;
                    var h = storyHeightRef;

                    if (el.Count > 1 && el[1].Length > 6)
                    {
                        var hVal = ParseDouble(GetPart(el[1], 6), -1.0);
                        if (hVal > 0.0)
                            h = NormalizeColumnHeight(hVal, storyHeightRef, storyBase, storyTop);
                    }

                    var axisMapIndex = elementNo % 100;
                    if (axisMapIndex <= 0)
                        axisMapIndex = i + 1;

                    if (!colMapping.TryGetValue(axisMapIndex, out var map))
                    {
                        // Some ST4 files define column-axis mapping once (plan positions),
                        // but repeat columns for upper stories in /Columns Data.
                        // Reuse axis mapping cyclically so upper-story columns are not dropped.
                        if (colMapping.TryGetValue(i + 1, out map))
                        {
                            // matched by raw sequence index
                        }
                        else if (colMapping.Count == 0)
                            continue;
                        else
                        {
                            var mappedIndex = (i % colMapping.Count) + 1;
                            if (!colMapping.TryGetValue(mappedIndex, out map))
                                continue;
                        }
                    }

                    if (map.xId == 0 || map.yId == 0)
                    {
                        if (colMapping.Count == 0)
                            continue;
                    }

                    columns.Add(new ColumnData(name, storyNo, storyTop, map.xId, map.yId, b, d, h, map.eccX, map.eccY, map.rot));
                }

                var beams = new List<BeamData>();
                var beamBlocks = GroupElements(beamLines);
                foreach (var el in beamBlocks)
                {
                    if (el.Count == 0)
                        continue;

                    var p1 = el[0];
                    var elementNo = ParseInt(GetPart(p1, 0), 0);
                    var name = "K" + GetPart(p1, 0);
                    var storyNo = Math.Max(1, elementNo / 100);
                    var storyTop = GetStoryTop(storyTopByNo, storyNo, zTop);
                    var b = ParseDouble(GetPart(p1, 1), 0.0) / 100.0;
                    var h = ParseDouble(GetPart(p1, 2), 0.0) / 100.0;
                    var axis = ParseInt(GetPart(p1, 4), 0);
                    var start = ParseInt(GetPart(p1, 5), 0);
                    var end = ParseInt(GetPart(p1, 6), 0);
                    var ecc = ParseInt(GetPart(p1, 7), 0);
                    var dz = ParseDouble(GetPart(p1, 11), 0.0) / 100.0;

                    beams.Add(new BeamData(name, storyNo, storyTop, axis, start, end, b, h, ecc, dz));
                }

                var slabs = new List<SlabData>();
                var slabBlocks = GroupElements(slabLines);
                foreach (var el in slabBlocks)
                {
                    if (el.Count == 0)
                        continue;

                    var p1 = el[0];
                    var elementNo = ParseInt(GetPart(p1, 0), 0);
                    var name = "D" + GetPart(p1, 0);
                    var storyNo = Math.Max(1, elementNo / 100);
                    var storyTop = GetStoryTop(storyTopByNo, storyNo, zTop);
                    var thick = ParseDouble(GetPart(p1, 1), 0.0) / 100.0;
                    var slabType = ParseInt(GetPart(p1, 4), 0);
                    var dropStart = ParseDouble(GetPart(p1, 5), 0.0) / 100.0;
                    var dropEnd = ParseSlabEndDrop(p1, dropStart);
                    var stairTag = ParseInt(GetPart(p1, 14), 0);
                    var stairFlag = ParseInt(GetPart(p1, 24), 0);

                    slabs.Add(new SlabData(
                        name,
                        storyNo,
                        storyTop,
                        slabType,
                        stairTag,
                        stairFlag,
                        ParseInt(GetPart(p1, 10), 0),
                        ParseInt(GetPart(p1, 11), 0),
                        ParseInt(GetPart(p1, 8), 0),
                        ParseInt(GetPart(p1, 9), 0),
                        thick,
                        dropStart,
                        dropEnd
                    ));
                }

                BuildNeighborhoods(columns, beams, slabs, xBase);
                model = new St4Model(zTop, storyTopByNo, yCoords, xBase, nodes, columns, beams, slabs);
                return true;
            }
        }

        private static class CadRenderer
        {
            public static void Render(Document doc, Database db, Editor ed, St4Model model)
            {
                var layers = new[] { "KOLONLAR", "KIRISLER", "DOSEMELER", "AKSLAR", "ETIKETLER" };
                using (doc.LockDocument())
                using (var tr = db.TransactionManager.StartTransaction())
                {
                    var layerTable = (LayerTable)tr.GetObject(db.LayerTableId, OpenMode.ForRead);
                    EnsureLayer(db, tr, layerTable, "KOLONLAR", 1);
                    EnsureLayer(db, tr, layerTable, "KIRISLER", 5);
                    EnsureLayer(db, tr, layerTable, "DOSEMELER", 3);
                    EnsureLayer(db, tr, layerTable, "AKSLAR", 2);
                    EnsureLayer(db, tr, layerTable, "ETIKETLER", 4);

                    DeleteObjectsOnLayers(db, tr, new HashSet<string>(layers, StringComparer.OrdinalIgnoreCase));
                    var modelSpace = GetModelSpace(db, tr);

                    try
                    {
                        DrawAxes(modelSpace, model.YCoords, model.XBase, model.Nodes, "AKSLAR", "ETIKETLER", ToCadZ(model.ZTop) + 0.02);
                    }
                    catch (System.Exception ex)
                    {
                        ed.WriteMessage($"\nAks cizimi atlandi: {ex.Message}");
                    }

                    var aAxisId = model.XBase.Count > 0 ? model.XBase.Keys.Min() : 0;
                    var hasAAxisNormal = false;
                    var aAxisInwardNormal = new Vector2d(0.0, 0.0);
                    if (model.XBase.Count > 0 && model.YCoords.Count > 1 && model.Nodes.Count > 0)
                    {
                        var yOrdered = model.YCoords.Keys.OrderBy(id => id).ToList();
                        var aAxisPoints = yOrdered
                            .Where(yId => model.Nodes.ContainsKey((aAxisId, yId)))
                            .Select(yId => model.Nodes[(aAxisId, yId)])
                            .ToList();
                        if (aAxisPoints.Count >= 2)
                        {
                            var pStart = aAxisPoints.First();
                            var pEnd = aAxisPoints.Last();
                            var adx = pEnd.X - pStart.X;
                            var ady = pEnd.Y - pStart.Y;
                            var alen = Math.Sqrt((adx * adx) + (ady * ady));
                            if (alen > 1e-12)
                            {
                                var n1 = new Vector2d(-ady / alen, adx / alen);
                                var n2 = new Vector2d(ady / alen, -adx / alen);
                                var mid = new Point2d((pStart.X + pEnd.X) / 2.0, (pStart.Y + pEnd.Y) / 2.0);
                                var modelCenter = new Point2d(
                                    model.Nodes.Values.Average(p => p.X),
                                    model.Nodes.Values.Average(p => p.Y));
                                var toCenter = new Vector2d(modelCenter.X - mid.X, modelCenter.Y - mid.Y);
                                aAxisInwardNormal = Dot(n1, toCenter) >= Dot(n2, toCenter) ? n1 : n2;
                                hasAAxisNormal = true;
                            }
                        }
                    }

                    foreach (var c in model.Columns)
                    {
                        try
                        {
                            c.Draw(modelSpace, tr, model.Nodes, aAxisId, hasAAxisNormal, aAxisInwardNormal);
                        }
                        catch (System.Exception ex)
                        {
                            ed.WriteMessage($"\nKolon atlandi ({c.Name}): {ex.Message}");
                        }
                    }

                    foreach (var b in model.Beams)
                    {
                        try
                        {
                            b.Draw(modelSpace, tr, model.Nodes, model.XBase);
                        }
                        catch (System.Exception ex)
                        {
                            ed.WriteMessage($"\nKiris atlandi ({b.Name}): {ex.Message}");
                        }
                    }

                    foreach (var s in model.Slabs)
                    {
                        try
                        {
                            s.Draw(modelSpace, tr, model.Nodes, model.XBase, model.Beams);
                        }
                        catch (System.Exception ex)
                        {
                            ed.WriteMessage($"\nDoseme atlandi ({s.Name}): {ex.Message}");
                        }
                    }

                    tr.Commit();
                }
            }
        }

        private static Dictionary<string, List<string>> ParseSections(IEnumerable<string> lines)
        {
            var sections = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
            var current = string.Empty;

            foreach (var raw in lines)
            {
                var line = (raw ?? string.Empty).Trim();
                if (string.IsNullOrEmpty(line))
                    continue;

                if (line.StartsWith("/", StringComparison.Ordinal))
                {
                    current = line;
                    if (!sections.ContainsKey(current))
                        sections[current] = new List<string>();
                    continue;
                }

                if (!string.IsNullOrEmpty(current))
                    sections[current].Add(line);
            }

            return sections;
        }

        private static List<string> GetSection(Dictionary<string, List<string>> sections, string namePart)
        {
            foreach (var kv in sections)
            {
                if (kv.Key.IndexOf(namePart, StringComparison.OrdinalIgnoreCase) >= 0)
                    return kv.Value;
            }

            return new List<string>();
        }

        private static List<List<string[]>> GroupElements(List<string> linesArray)
        {
            var elements = new List<List<string[]>>();
            var current = new List<string[]>();

            foreach (var line in linesArray)
            {
                var parts = line.Split(',');
                var isNew = parts.Length > 2 &&
                            int.TryParse(parts[0].Trim(), NumberStyles.Integer, CultureInfo.InvariantCulture, out var id) &&
                            id > 0;

                if (isNew)
                {
                    if (current.Count > 0)
                        elements.Add(current);
                    current = new List<string[]> { parts };
                }
                else if (current.Count > 0)
                {
                    current.Add(parts);
                }
            }

            if (current.Count > 0)
                elements.Add(current);

            return elements;
        }

        private static string GetPart(string[] arr, int index)
        {
            if (arr == null || index < 0 || index >= arr.Length)
                return "0";
            return arr[index].Trim();
        }

        private static int ParseInt(string value, int fallback)
        {
            if (int.TryParse(value?.Trim(), NumberStyles.Integer, CultureInfo.InvariantCulture, out var result))
                return result;
            if (int.TryParse(value?.Trim(), NumberStyles.Integer, CultureInfo.CurrentCulture, out result))
                return result;
            return fallback;
        }

        private static double ParseDouble(string value, double fallback)
        {
            if (double.TryParse(value?.Trim(), NumberStyles.Float, CultureInfo.InvariantCulture, out var result))
                return result;
            if (double.TryParse(value?.Trim(), NumberStyles.Float, CultureInfo.CurrentCulture, out result))
                return result;
            return fallback;
        }

        private static double ParseStoryTopLevel(List<string> storyData, double fallback)
        {
            if (storyData == null || storyData.Count == 0)
                return fallback;

            var rows = storyData
                .Where(l => !string.IsNullOrWhiteSpace(l) && l.Contains(","))
                .ToList();
            if (rows.Count == 0)
                return fallback;

            var firstColumnValues = new List<double>();
            var thirdColumnValues = new List<double>();

            foreach (var row in rows)
            {
                var parts = row.Split(',');
                if (parts.Length >= 1)
                {
                    var v1 = ParseDouble(GetPart(parts, 0), 0.0);
                    if (v1 > 0.05 && v1 < 100.0)
                        firstColumnValues.Add(v1);
                }

                if (parts.Length >= 3)
                {
                    var v3 = ParseDouble(GetPart(parts, 2), 0.0);
                    if (v3 > 0.05 && v3 < 100.0)
                        thirdColumnValues.Add(v3);
                }
            }

            if (firstColumnValues.Count > 0)
                return firstColumnValues.Max();
            if (thirdColumnValues.Count > 0)
                return thirdColumnValues.Max();

            return fallback;
        }

        private static List<double> ParseStoryTops(List<string> storyData, double fallbackTop)
        {
            var tops = new List<double>();
            if (storyData != null)
            {
                foreach (var row in storyData)
                {
                    if (string.IsNullOrWhiteSpace(row) || !row.Contains(","))
                        continue;

                    var parts = row.Split(',');
                    if (parts.Length < 2)
                        continue;

                    var idx = ParseInt(GetPart(parts, 1), int.MinValue);
                    if (idx < 0)
                        continue;

                    var top = ParseDouble(GetPart(parts, 0), double.NaN);
                    if (double.IsNaN(top) || top < -1000.0 || top > 10000.0)
                        continue;

                    tops.Add(top);
                }
            }

            if (tops.Count == 0)
                tops.Add(Math.Max(0.0, fallbackTop));

            return tops
                .Distinct()
                .OrderBy(v => v)
                .ToList();
        }

        private static Dictionary<int, double> BuildStoryTopByNumber(List<double> storyTops)
        {
            var map = new Dictionary<int, double>();
            if (storyTops == null || storyTops.Count == 0)
            {
                map[1] = 3.0;
                return map;
            }

            if (storyTops.Count == 1)
            {
                map[1] = storyTops[0];
                return map;
            }

            for (var i = 1; i < storyTops.Count; i++)
                map[i] = storyTops[i];

            return map;
        }

        private static double GetStoryTop(Dictionary<int, double> storyTopByNo, int storyNo, double fallback)
        {
            if (storyTopByNo != null && storyTopByNo.TryGetValue(storyNo, out var top))
                return top;
            return fallback;
        }

        private static double GetStoryHeight(Dictionary<int, double> storyTopByNo, int storyNo, double fallback)
        {
            if (storyTopByNo == null || storyTopByNo.Count == 0)
                return fallback;

            if (!storyTopByNo.TryGetValue(storyNo, out var top))
                return fallback;

            var prevTop = storyNo > 1 && storyTopByNo.TryGetValue(storyNo - 1, out var prev)
                ? prev
                : 0.0;
            var h = top - prevTop;
            if (h > 0.05)
                return h;
            return fallback;
        }

        private static double ParseSlabEndDrop(string[] parts, double dropStart)
        {
            var c15 = ParseDouble(GetPart(parts, 15), double.NaN);
            var c16 = ParseDouble(GetPart(parts, 16), double.NaN);

            var options = new List<double>();
            if (!double.IsNaN(c15))
                options.Add(c15 / 100.0);
            if (!double.IsNaN(c16))
                options.Add(c16 / 100.0);

            if (options.Count == 0)
                return dropStart;

            // Prefer candidate that differs from start; otherwise prefer non-zero.
            var diff = options
                .Where(v => Math.Abs(v - dropStart) > 1e-9)
                .OrderByDescending(v => Math.Abs(v - dropStart))
                .FirstOrDefault();
            if (!double.IsNaN(diff))
                return diff;

            return options
                .OrderByDescending(v => Math.Abs(v))
                .FirstOrDefault(dropStart);
        }

        private static void EnsureLayer(Database db, Transaction tr, LayerTable layerTable, string name, short aciColor)
        {
            if (layerTable.Has(name))
                return;

            layerTable.UpgradeOpen();
            var layer = new LayerTableRecord
            {
                Name = name,
                Color = Color.FromColorIndex(ColorMethod.ByAci, aciColor)
            };
            layerTable.Add(layer);
            tr.AddNewlyCreatedDBObject(layer, true);
        }

        private static BlockTableRecord GetModelSpace(Database db, Transaction tr)
        {
            var blockTable = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);
            return (BlockTableRecord)tr.GetObject(blockTable[BlockTableRecord.ModelSpace], OpenMode.ForWrite);
        }

        private static void DeleteObjectsOnLayers(Database db, Transaction tr, HashSet<string> layerNames)
        {
            var blockTable = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);
            var modelSpace = (BlockTableRecord)tr.GetObject(blockTable[BlockTableRecord.ModelSpace], OpenMode.ForWrite);

            var toDelete = new List<ObjectId>();
            foreach (ObjectId id in modelSpace)
            {
                var ent = tr.GetObject(id, OpenMode.ForRead) as Entity;
                if (ent == null)
                    continue;

                if (layerNames.Contains(ent.Layer))
                    toDelete.Add(id);
            }

            foreach (var id in toDelete)
            {
                var ent = tr.GetObject(id, OpenMode.ForWrite) as Entity;
                ent?.Erase();
            }
        }

        private static double GetColumnTopFromConnectedBeams(ColumnData c, double defaultTop)
        {
            var connectedBeamTops = c.ConnectedBeams
                .Select(b => defaultTop + b.Dz)
                .ToList();

            if (connectedBeamTops.Count == 0)
                return defaultTop;

            // If at least one connected beam is at story top, keep story top.
            // Otherwise, align the column top to the highest connected beam top.
            var highestConnectedTop = connectedBeamTops.Max();
            return highestConnectedTop < (defaultTop - 1e-6) ? highestConnectedTop : defaultTop;
        }

        private static void BuildNeighborhoods(
            List<ColumnData> columns,
            List<BeamData> beams,
            List<SlabData> slabs,
            Dictionary<int, double> xBase)
        {
            foreach (var c in columns)
            {
                c.ConnectedBeams.Clear();
                c.AdjacentSlabs.Clear();
            }

            foreach (var b in beams)
            {
                b.ConnectedColumns.Clear();
                b.NeighborBeams.Clear();
                b.AdjacentSlabs.Clear();
            }

            foreach (var s in slabs)
            {
                s.BoundaryBeams.Clear();
                s.NeighborBeams.Clear();
                s.NeighborSlabs.Clear();
            }

            var columnByNode = columns.ToDictionary(c => (c.StoryNo, c.XId, c.YId), c => c);
            var beamNodes = new Dictionary<BeamData, ((int xId, int yId) Start, (int xId, int yId) End)>();
            var verticalAxes = new HashSet<int>(xBase.Keys);

            foreach (var b in beams)
            {
                var nodes = GetBeamNodeKeys(b, verticalAxes);
                beamNodes[b] = nodes;

                if (columnByNode.TryGetValue((b.StoryNo, nodes.Start.xId, nodes.Start.yId), out var c1))
                {
                    c1.ConnectedBeams.Add(b);
                    b.ConnectedColumns.Add(c1);
                }

                if (columnByNode.TryGetValue((b.StoryNo, nodes.End.xId, nodes.End.yId), out var c2))
                {
                    if (!ReferenceEquals(c1, c2))
                    {
                        c2.ConnectedBeams.Add(b);
                        b.ConnectedColumns.Add(c2);
                    }
                }
            }

            var beamByNode = new Dictionary<(int storyNo, int xId, int yId), List<BeamData>>();
            foreach (var kvp in beamNodes)
            {
                AddBeamNodeLink(beamByNode, (kvp.Key.StoryNo, kvp.Value.Start.xId, kvp.Value.Start.yId), kvp.Key);
                AddBeamNodeLink(beamByNode, (kvp.Key.StoryNo, kvp.Value.End.xId, kvp.Value.End.yId), kvp.Key);
            }

            foreach (var nodeBeams in beamByNode.Values)
            {
                for (var i = 0; i < nodeBeams.Count; i++)
                {
                    for (var j = i + 1; j < nodeBeams.Count; j++)
                    {
                        var bi = nodeBeams[i];
                        var bj = nodeBeams[j];
                        if (!bi.NeighborBeams.Contains(bj))
                            bi.NeighborBeams.Add(bj);
                        if (!bj.NeighborBeams.Contains(bi))
                            bj.NeighborBeams.Add(bi);
                    }
                }
            }

            foreach (var s in slabs)
            {
                foreach (var b in beams)
                {
                    if (b.StoryNo != s.StoryNo)
                        continue;
                    if (!IsBeamOnSlabBoundary(b, s, verticalAxes))
                        continue;

                    s.BoundaryBeams.Add(b);
                    if (!s.NeighborBeams.Contains(b))
                        s.NeighborBeams.Add(b);
                    if (!b.AdjacentSlabs.Contains(s))
                        b.AdjacentSlabs.Add(s);
                }

                foreach (var c in columns)
                {
                    if (c.StoryNo != s.StoryNo)
                        continue;
                    if (IsColumnOnOrInsideSlab(c, s))
                        c.AdjacentSlabs.Add(s);
                }
            }

            foreach (var b in beams)
            {
                var adjacent = b.AdjacentSlabs;
                for (var i = 0; i < adjacent.Count; i++)
                {
                    for (var j = i + 1; j < adjacent.Count; j++)
                    {
                        var si = adjacent[i];
                        var sj = adjacent[j];
                        if (!si.NeighborSlabs.Contains(sj))
                            si.NeighborSlabs.Add(sj);
                        if (!sj.NeighborSlabs.Contains(si))
                            sj.NeighborSlabs.Add(si);
                    }
                }
            }
        }

        private static ((int xId, int yId) Start, (int xId, int yId) End) GetBeamNodeKeys(BeamData b, HashSet<int> verticalAxes)
        {
            var isVerticalBeam = verticalAxes.Contains(b.Axis);
            return isVerticalBeam
                ? ((b.Axis, b.Start), (b.Axis, b.End))
                : ((b.Start, b.Axis), (b.End, b.Axis));
        }

        private static void AddBeamNodeLink(
            Dictionary<(int storyNo, int xId, int yId), List<BeamData>> beamByNode,
            (int storyNo, int xId, int yId) node,
            BeamData beam)
        {
            if (!beamByNode.TryGetValue(node, out var list))
            {
                list = new List<BeamData>();
                beamByNode[node] = list;
            }

            list.Add(beam);
        }

        private static bool IsBeamOnSlabBoundary(BeamData b, SlabData s, HashSet<int> verticalAxes)
        {
            var minX = Math.Min(s.X1, s.X2);
            var maxX = Math.Max(s.X1, s.X2);
            var minY = Math.Min(s.Y1, s.Y2);
            var maxY = Math.Max(s.Y1, s.Y2);

            var isVerticalBeam = verticalAxes.Contains(b.Axis);
            if (isVerticalBeam)
            {
                if (b.Axis != minX && b.Axis != maxX)
                    return false;
                return RangesOverlapInclusive(minY, maxY, b.Start, b.End);
            }

            if (b.Axis != minY && b.Axis != maxY)
                return false;
            return RangesOverlapInclusive(minX, maxX, b.Start, b.End);
        }

        private static bool IsColumnOnOrInsideSlab(ColumnData c, SlabData s)
        {
            var minX = Math.Min(s.X1, s.X2);
            var maxX = Math.Max(s.X1, s.X2);
            var minY = Math.Min(s.Y1, s.Y2);
            var maxY = Math.Max(s.Y1, s.Y2);

            return c.XId >= minX && c.XId <= maxX &&
                   c.YId >= minY && c.YId <= maxY;
        }

        private static bool RangesOverlapInclusive(int a1, int a2, int b1, int b2)
        {
            var minA = Math.Min(a1, a2);
            var maxA = Math.Max(a1, a2);
            var minB = Math.Min(b1, b2);
            var maxB = Math.Max(b1, b2);
            return Math.Min(maxA, maxB) >= Math.Max(minA, minB);
        }

        private static void DrawAxes(
            BlockTableRecord modelSpace,
            Dictionary<int, double> yCoords,
            Dictionary<int, double> xBase,
            Dictionary<(int xId, int yId), Point2d> nodes,
            string axisLayer,
            string labelLayer,
            double axisZ)
        {
            if (yCoords.Count == 0 || xBase.Count == 0 || nodes.Count == 0)
                return;

            const double ext = 2.0;
            const double labelHeight = 0.20;

            var minYId = yCoords.Keys.Min();
            var maxYId = yCoords.Keys.Max();
            var orderedYIds = yCoords.Keys.OrderBy(id => id).ToList();
            var orderedXIds = xBase.Keys.OrderBy(id => id).ToList();

            // With current import transform, y-id families correspond to numeric axes (1,2,3...).
            for (var i = 0; i < orderedYIds.Count; i++)
            {
                var yId = orderedYIds[i];
                var family = new List<Point2d>();
                foreach (var xId in orderedXIds)
                {
                    if (nodes.TryGetValue((xId, yId), out var pt))
                        family.Add(pt);
                }
                if (family.Count < 2)
                    continue;

                var p1 = family.First();
                var p2 = family.Last();
                var dx = p2.X - p1.X;
                var dy = p2.Y - p1.Y;
                var len = Math.Sqrt((dx * dx) + (dy * dy));
                if (len <= 1e-12)
                    continue;

                var ux = dx / len;
                var uy = dy / len;
                var s = new Point3d(p1.X - (ux * ext), p1.Y - (uy * ext), axisZ);
                var e = new Point3d(p2.X + (ux * ext), p2.Y + (uy * ext), axisZ);
                var axisLabel = (i + 1).ToString(CultureInfo.InvariantCulture);

                var ln = new Line(s, e) { Layer = axisLayer };
                modelSpace.AppendEntity(ln);

                var t1 = new DBText { Layer = labelLayer, Height = labelHeight, Position = s, TextString = axisLabel };
                var t2 = new DBText { Layer = labelLayer, Height = labelHeight, Position = e, TextString = axisLabel };
                modelSpace.AppendEntity(t1);
                modelSpace.AppendEntity(t2);
            }

            // With current import transform, x-id families correspond to lettered axes (A,B,C...).
            for (var i = 0; i < orderedXIds.Count; i++)
            {
                var xId = orderedXIds[i];
                var family = new List<Point2d>();
                foreach (var yId in orderedYIds)
                {
                    if (nodes.TryGetValue((xId, yId), out var pt))
                        family.Add(pt);
                }
                if (family.Count < 2)
                    continue;

                var p1 = family.First();
                var p2 = family.Last();
                var dx = p2.X - p1.X;
                var dy = p2.Y - p1.Y;
                var len = Math.Sqrt((dx * dx) + (dy * dy));
                if (len <= 1e-12)
                    continue;

                var ux = dx / len;
                var uy = dy / len;
                var s = new Point3d(p1.X - (ux * ext), p1.Y - (uy * ext), axisZ);
                var e = new Point3d(p2.X + (ux * ext), p2.Y + (uy * ext), axisZ);
                var axisLabel = AxisLetterFromIndex(i);

                var ln = new Line(s, e) { Layer = axisLayer };
                modelSpace.AppendEntity(ln);

                var t1 = new DBText { Layer = labelLayer, Height = labelHeight, Position = s, TextString = axisLabel };
                var t2 = new DBText { Layer = labelLayer, Height = labelHeight, Position = e, TextString = axisLabel };
                modelSpace.AppendEntity(t1);
                modelSpace.AppendEntity(t2);
            }
        }

        private static string AxisLetterFromIndex(int index)
        {
            if (index < 0)
                return "?";

            var letters = string.Empty;
            var value = index;
            do
            {
                letters = (char)('A' + (value % 26)) + letters;
                value = (value / 26) - 1;
            } while (value >= 0);

            return letters;
        }

        private static Point2d OffsetTowardCentroid(Point2d p, double cx, double cy, double dist = 0.20)
        {
            var dx = cx - p.X;
            var dy = cy - p.Y;
            var len = Math.Sqrt((dx * dx) + (dy * dy));
            if (len <= 1e-12)
                return p;

            var move = Math.Min(dist, len * 0.4);
            return new Point2d(p.X + (dx / len) * move, p.Y + (dy / len) * move);
        }

        private static (double Ox, double Oy) ComputeBeamEccOffset(double dx, double dy, double width, int ecc)
        {
            if (Math.Abs(width) <= 1e-12 || ecc == 0)
                return (0.0, 0.0);

            var len = Math.Sqrt((dx * dx) + (dy * dy));
            if (len <= 1e-12)
                return (0.0, 0.0);

            // Local left normal of beam centerline.
            var nx = -dy / len;
            var ny = dx / len;

            // Keep existing user-calibrated sign multiplier, but now apply it in local coordinates.
            var sideSign = ecc == -1 ? 1.0 : -1.0;
            var dist = (width / 2.0) * sideSign * BeamEccSignMultiplier;
            return (nx * dist, ny * dist);
        }

        private static double GetBoundaryBeamHalfWidth(
            IEnumerable<BeamData> beams,
            HashSet<int> verticalAxes,
            bool boundaryIsVertical,
            int axisId,
            int rangeStart,
            int rangeEnd)
        {
            var min = Math.Min(rangeStart, rangeEnd);
            var max = Math.Max(rangeStart, rangeEnd);

            var widths = beams
                .Where(b =>
                {
                    var isVerticalBeam = verticalAxes.Contains(b.Axis);
                    if (isVerticalBeam != boundaryIsVertical)
                        return false;
                    if (b.Axis != axisId)
                        return false;

                    var bMin = Math.Min(b.Start, b.End);
                    var bMax = Math.Max(b.Start, b.End);
                    return bMax >= min && bMin <= max;
                })
                .Select(b => b.B / 2.0)
                .Where(v => v > 1e-6)
                .ToList();

            return widths.Count == 0 ? 0.0 : widths.Max();
        }

        private static bool TryBuildSlabCornersFromBeams(
            Point2d p1,
            Point2d p2,
            Point2d p3,
            Point2d p4,
            Dictionary<(int xId, int yId), Point2d> nodes,
            Dictionary<int, double> xBase,
            IEnumerable<BeamData> beams,
            out List<Point2d> corners)
        {
            corners = null;

            var inside = new Point2d(
                (p1.X + p2.X + p3.X + p4.X) / 4.0,
                (p1.Y + p2.Y + p3.Y + p4.Y) / 4.0);

            var ordered = OrderCornersClockwise(new[] { p1, p2, p3, p4 });
            if (ordered.Count != 4)
                return false;

            if (!TryGetBeamInnerLineForEdge(ordered[0], ordered[1], inside, beams, nodes, xBase, out var l1) ||
                !TryGetBeamInnerLineForEdge(ordered[1], ordered[2], inside, beams, nodes, xBase, out var l2) ||
                !TryGetBeamInnerLineForEdge(ordered[2], ordered[3], inside, beams, nodes, xBase, out var l3) ||
                !TryGetBeamInnerLineForEdge(ordered[3], ordered[0], inside, beams, nodes, xBase, out var l4))
                return false;

            if (!TryIntersectLines(l1, l2, out var c1) ||
                !TryIntersectLines(l2, l3, out var c2) ||
                !TryIntersectLines(l3, l4, out var c3) ||
                !TryIntersectLines(l4, l1, out var c4))
                return false;

            corners = OrderCornersClockwise(new[] { c1, c2, c3, c4 });
            return corners.Count == 4;
        }

        private static bool TryBuildSlabCornersFromAxisBoundaries(
            SlabData slab,
            Dictionary<(int xId, int yId), Point2d> nodes,
            Dictionary<int, double> xBase,
            IEnumerable<BeamData> beams,
            out List<Point2d> corners)
        {
            corners = null;
            if (!nodes.TryGetValue((slab.X1, slab.Y1), out var p1) ||
                !nodes.TryGetValue((slab.X2, slab.Y1), out var p2) ||
                !nodes.TryGetValue((slab.X2, slab.Y2), out var p3) ||
                !nodes.TryGetValue((slab.X1, slab.Y2), out var p4))
                return false;

            var inside = new Point2d(
                (p1.X + p2.X + p3.X + p4.X) / 4.0,
                (p1.Y + p2.Y + p3.Y + p4.Y) / 4.0);
            var verticalAxes = new HashSet<int>(xBase.Keys);

            if (!TryGetBeamBoundaryInnerLineAxis(beams, nodes, verticalAxes, true, slab.X1, slab.Y1, slab.Y2, inside, out var left) ||
                !TryGetBeamBoundaryInnerLineAxis(beams, nodes, verticalAxes, true, slab.X2, slab.Y1, slab.Y2, inside, out var right) ||
                !TryGetBeamBoundaryInnerLineAxis(beams, nodes, verticalAxes, false, slab.Y1, slab.X1, slab.X2, inside, out var bottom) ||
                !TryGetBeamBoundaryInnerLineAxis(beams, nodes, verticalAxes, false, slab.Y2, slab.X1, slab.X2, inside, out var top))
                return false;

            if (!TryIntersectLines(left, bottom, out var c1) ||
                !TryIntersectLines(bottom, right, out var c2) ||
                !TryIntersectLines(right, top, out var c3) ||
                !TryIntersectLines(top, left, out var c4))
                return false;

            corners = OrderCornersClockwise(new[] { c1, c2, c3, c4 });
            return corners.Count == 4;
        }

        private static bool TryGetBeamBoundaryInnerLineAxis(
            IEnumerable<BeamData> beams,
            Dictionary<(int xId, int yId), Point2d> nodes,
            HashSet<int> verticalAxes,
            bool boundaryIsVertical,
            int axisId,
            int rangeStart,
            int rangeEnd,
            Point2d insidePoint,
            out (Point2d Point, Vector2d Dir) innerLine)
        {
            innerLine = default;
            var min = Math.Min(rangeStart, rangeEnd);
            var max = Math.Max(rangeStart, rangeEnd);
            var found = false;
            var bestOverlap = double.NegativeInfinity;
            var bestDist = double.PositiveInfinity;

            foreach (var b in beams)
            {
                var isVerticalBeam = verticalAxes.Contains(b.Axis);
                if (isVerticalBeam != boundaryIsVertical || b.Axis != axisId)
                    continue;

                var bMin = Math.Min(b.Start, b.End);
                var bMax = Math.Max(b.Start, b.End);
                var overlap = Math.Min(max, bMax) - Math.Max(min, bMin);
                if (overlap < -1e-9)
                    continue;

                if (!TryGetBeamCenterLine(b, nodes, verticalAxes, out var c1, out var c2, out var width))
                    continue;

                var dx = c2.X - c1.X;
                var dy = c2.Y - c1.Y;
                var len = Math.Sqrt((dx * dx) + (dy * dy));
                if (len <= 1e-12)
                    continue;

                var dir = new Vector2d(dx / len, dy / len);
                var nLeft = new Vector2d(-dir.Y, dir.X);
                var nRight = new Vector2d(dir.Y, -dir.X);
                var mid = new Point2d((c1.X + c2.X) / 2.0, (c1.Y + c2.Y) / 2.0);
                var toInside = new Vector2d(insidePoint.X - mid.X, insidePoint.Y - mid.Y);
                var inward = Dot(nLeft, toInside) >= Dot(nRight, toInside) ? nLeft : nRight;
                var candidate = (new Point2d(c1.X + inward.X * (width / 2.0), c1.Y + inward.Y * (width / 2.0)), dir);

                var edgeMidDist = DistancePointToLine(insidePoint, c1, c2);
                if (!found || overlap > bestOverlap || (Math.Abs(overlap - bestOverlap) <= 1e-9 && edgeMidDist < bestDist))
                {
                    innerLine = candidate;
                    bestOverlap = overlap;
                    bestDist = edgeMidDist;
                    found = true;
                }
            }

            return found;
        }

        private static bool TryGetBeamInnerLineForEdge(
            Point2d edgeA,
            Point2d edgeB,
            Point2d insidePoint,
            IEnumerable<BeamData> beams,
            Dictionary<(int xId, int yId), Point2d> nodes,
            Dictionary<int, double> xBase,
            out (Point2d Point, Vector2d Dir) innerLine)
        {
            innerLine = default;
            var edgeDirRaw = new Vector2d(edgeB.X - edgeA.X, edgeB.Y - edgeA.Y);
            var edgeLen = edgeDirRaw.Length;
            if (edgeLen <= 1e-12)
                return false;

            var edgeDir = new Vector2d(edgeDirRaw.X / edgeLen, edgeDirRaw.Y / edgeLen);
            var edgeMid = new Point2d((edgeA.X + edgeB.X) / 2.0, (edgeA.Y + edgeB.Y) / 2.0);
            var verticalAxes = new HashSet<int>(xBase.Keys);

            var found = false;
            var bestScore = double.NegativeInfinity;
            var foundLoose = false;
            var bestLooseScore = double.NegativeInfinity;
            (Point2d Point, Vector2d Dir) looseLine = default;
            foreach (var b in beams)
            {
                if (!TryGetBeamCenterLine(b, nodes, verticalAxes, out var c1, out var c2, out var width))
                    continue;

                var bdx = c2.X - c1.X;
                var bdy = c2.Y - c1.Y;
                var blen = Math.Sqrt((bdx * bdx) + (bdy * bdy));
                if (blen <= 1e-12)
                    continue;

                var beamDir = new Vector2d(bdx / blen, bdy / blen);
                var parallel = Math.Abs(Cross(edgeDir, beamDir));
                if (parallel > 0.60)
                    continue;

                var dist = DistancePointToLine(edgeMid, c1, c2);
                var overlap = ProjectedOverlap(edgeA, edgeB, c1, c2, edgeDir);
                var hasGoodOverlap = overlap >= (edgeLen * 0.10);

                var score = (2.5 * overlap) - (2.0 * dist) - (3.0 * parallel * edgeLen);
                var nLeft = new Vector2d(-beamDir.Y, beamDir.X);
                var nRight = new Vector2d(beamDir.Y, -beamDir.X);
                var mid = new Point2d((c1.X + c2.X) / 2.0, (c1.Y + c2.Y) / 2.0);
                var toInside = new Vector2d(insidePoint.X - mid.X, insidePoint.Y - mid.Y);
                var inward = Dot(nLeft, toInside) >= Dot(nRight, toInside) ? nLeft : nRight;
                var candidateLine = (
                    new Point2d(c1.X + inward.X * (width / 2.0), c1.Y + inward.Y * (width / 2.0)),
                    beamDir);

                if (hasGoodOverlap && score > bestScore)
                {
                    innerLine = candidateLine;
                    bestScore = score;
                    found = true;
                }

                // Keep a loose fallback candidate (closest near-parallel beam).
                var looseScore = -(dist + (parallel * edgeLen));
                if (!foundLoose || looseScore > bestLooseScore)
                {
                    looseLine = candidateLine;
                    bestLooseScore = looseScore;
                    foundLoose = true;
                }
            }

            if (!found && foundLoose)
            {
                innerLine = looseLine;
                return true;
            }

            return found;
        }

        private static bool TryGetBeamCenterLine(
            BeamData b,
            Dictionary<(int xId, int yId), Point2d> nodes,
            HashSet<int> verticalAxes,
            out Point2d c1,
            out Point2d c2,
            out double width)
        {
            width = b.B;
            c1 = default;
            c2 = default;

            Point2d n1;
            Point2d n2;
            var isVerticalBeam = verticalAxes.Contains(b.Axis);
            if (isVerticalBeam)
            {
                if (!nodes.TryGetValue((b.Axis, b.Start), out n1) || !nodes.TryGetValue((b.Axis, b.End), out n2))
                    return false;
            }
            else
            {
                if (!nodes.TryGetValue((b.Start, b.Axis), out n1) || !nodes.TryGetValue((b.End, b.Axis), out n2))
                    return false;
            }

            var dx = n2.X - n1.X;
            var dy = n2.Y - n1.Y;
            var (ox, oy) = ComputeBeamEccOffset(dx, dy, b.B, b.Ecc);
            c1 = new Point2d(n1.X + ox, n1.Y + oy);
            c2 = new Point2d(n2.X + ox, n2.Y + oy);
            return true;
        }

        private static double Cross(Vector2d a, Vector2d b) => (a.X * b.Y) - (a.Y * b.X);

        private static double DistancePointToLine(Point2d p, Point2d a, Point2d b)
        {
            var ab = new Vector2d(b.X - a.X, b.Y - a.Y);
            var len = ab.Length;
            if (len <= 1e-12)
                return 0.0;
            return Math.Abs(((p.X - a.X) * ab.Y) - ((p.Y - a.Y) * ab.X)) / len;
        }

        private static double ProjectedOverlap(Point2d a1, Point2d a2, Point2d b1, Point2d b2, Vector2d axisUnit)
        {
            var aMin = Math.Min((a1.X * axisUnit.X) + (a1.Y * axisUnit.Y), (a2.X * axisUnit.X) + (a2.Y * axisUnit.Y));
            var aMax = Math.Max((a1.X * axisUnit.X) + (a1.Y * axisUnit.Y), (a2.X * axisUnit.X) + (a2.Y * axisUnit.Y));
            var bMin = Math.Min((b1.X * axisUnit.X) + (b1.Y * axisUnit.Y), (b2.X * axisUnit.X) + (b2.Y * axisUnit.Y));
            var bMax = Math.Max((b1.X * axisUnit.X) + (b1.Y * axisUnit.Y), (b2.X * axisUnit.X) + (b2.Y * axisUnit.Y));
            return Math.Max(0.0, Math.Min(aMax, bMax) - Math.Max(aMin, bMin));
        }

        private static List<Point2d> BuildInsetSlabCorners(
            Point2d p1,
            Point2d p2,
            Point2d p3,
            Point2d p4,
            double leftInset,
            double rightInset,
            double bottomInset,
            double topInset)
        {
            var centroid = new Point2d(
                (p1.X + p2.X + p3.X + p4.X) / 4.0,
                (p1.Y + p2.Y + p3.Y + p4.Y) / 4.0);

            var b = OffsetEdgeTowardPoint(p1, p2, bottomInset, centroid);
            var r = OffsetEdgeTowardPoint(p2, p3, rightInset, centroid);
            var t = OffsetEdgeTowardPoint(p3, p4, topInset, centroid);
            var l = OffsetEdgeTowardPoint(p4, p1, leftInset, centroid);

            if (TryIntersectLines(l, b, out var c1) &&
                TryIntersectLines(b, r, out var c2) &&
                TryIntersectLines(r, t, out var c3) &&
                TryIntersectLines(t, l, out var c4))
            {
                return OrderCornersClockwise(new[] { c1, c2, c3, c4 });
            }

            // Fallback for degenerate/near-parallel geometries.
            return OrderCornersClockwise(new[]
            {
                OffsetTowardCentroid(p1, centroid.X, centroid.Y, Math.Min(leftInset, bottomInset)),
                OffsetTowardCentroid(p2, centroid.X, centroid.Y, Math.Min(rightInset, bottomInset)),
                OffsetTowardCentroid(p3, centroid.X, centroid.Y, Math.Min(rightInset, topInset)),
                OffsetTowardCentroid(p4, centroid.X, centroid.Y, Math.Min(leftInset, topInset))
            });
        }

        private static (Point2d Point, Vector2d Dir) OffsetEdgeTowardPoint(
            Point2d a,
            Point2d b,
            double offset,
            Point2d insidePoint)
        {
            var edge = new Vector2d(b.X - a.X, b.Y - a.Y);
            var len = edge.Length;
            if (len <= 1e-12)
                return (a, new Vector2d(1.0, 0.0));

            var dir = new Vector2d(edge.X / len, edge.Y / len);
            var normal1 = new Vector2d(-dir.Y, dir.X);
            var normal2 = new Vector2d(dir.Y, -dir.X);
            var mid = new Point2d((a.X + b.X) / 2.0, (a.Y + b.Y) / 2.0);
            var toInside = new Vector2d(insidePoint.X - mid.X, insidePoint.Y - mid.Y);
            var useNormal = Dot(normal1, toInside) >= Dot(normal2, toInside) ? normal1 : normal2;
            var move = Math.Max(0.0, offset);

            return (new Point2d(a.X + (useNormal.X * move), a.Y + (useNormal.Y * move)), dir);
        }

        private static bool TryIntersectLines(
            (Point2d Point, Vector2d Dir) l1,
            (Point2d Point, Vector2d Dir) l2,
            out Point2d intersection)
        {
            var det = (l1.Dir.X * l2.Dir.Y) - (l1.Dir.Y * l2.Dir.X);
            if (Math.Abs(det) <= 1e-10)
            {
                intersection = default;
                return false;
            }

            var dx = l2.Point.X - l1.Point.X;
            var dy = l2.Point.Y - l1.Point.Y;
            var t = ((dx * l2.Dir.Y) - (dy * l2.Dir.X)) / det;
            intersection = new Point2d(
                l1.Point.X + (l1.Dir.X * t),
                l1.Point.Y + (l1.Dir.Y * t));
            return true;
        }

        private static double Dot(Vector2d a, Vector2d b)
        {
            return (a.X * b.X) + (a.Y * b.Y);
        }

        private static void AlignSolidCenter(Solid3d solid, Point3d targetCenter)
        {
            var ext = solid.GeometricExtents;
            var actualCenter = new Point3d(
                (ext.MinPoint.X + ext.MaxPoint.X) / 2.0,
                (ext.MinPoint.Y + ext.MaxPoint.Y) / 2.0,
                (ext.MinPoint.Z + ext.MaxPoint.Z) / 2.0);

            solid.TransformBy(Matrix3d.Displacement(new Vector3d(
                targetCenter.X - actualCenter.X,
                targetCenter.Y - actualCenter.Y,
                targetCenter.Z - actualCenter.Z)));
        }

        private static List<Point2d> OrderCornersClockwise(IEnumerable<Point2d> corners)
        {
            var pts = corners.ToList();
            if (pts.Count < 3)
                return pts;

            var cx = pts.Average(p => p.X);
            var cy = pts.Average(p => p.Y);

            return pts
                .OrderByDescending(p => Math.Atan2(p.Y - cy, p.X - cx))
                .ToList();
        }

        private static double PolygonAreaAbs(IReadOnlyList<Point2d> pts)
        {
            if (pts == null || pts.Count < 3)
                return 0.0;

            double area2 = 0.0;
            for (var i = 0; i < pts.Count; i++)
            {
                var j = (i + 1) % pts.Count;
                area2 += (pts[i].X * pts[j].Y) - (pts[j].X * pts[i].Y);
            }

            return Math.Abs(area2) * 0.5;
        }

        private static bool TryCreateSlopedSlabFaces(
            BlockTableRecord modelSpace,
            Transaction tr,
            IReadOnlyList<Point2d> corners,
            double thickness,
            double topStart,
            double topEnd,
            int slopeDirectionHint)
        {
            if (corners == null || corners.Count < 4 || thickness <= 1e-9 || PolygonAreaAbs(corners) <= 1e-9)
                return false;

            var c = OrderCornersClockwise(corners).Take(4).ToList();
            if (c.Count < 4)
                return false;

            var top = new Point3d[4];
            var bot = new Point3d[4];

            var topByCorner = new double[4];
            // Determine primary slab direction from geometry, then apply hint as forward/reverse.
            var e01 = new Vector2d(c[1].X - c[0].X, c[1].Y - c[0].Y);
            var e12 = new Vector2d(c[2].X - c[1].X, c[2].Y - c[1].Y);
            var primaryDir = e01.Length >= e12.Length ? e01 : e12;
            if (primaryDir.Length <= 1e-12)
                return false;
            primaryDir = new Vector2d(primaryDir.X / primaryDir.Length, primaryDir.Y / primaryDir.Length);
            var reverse = slopeDirectionHint == 2 || slopeDirectionHint == 4;
            var useHint = slopeDirectionHint >= 1 && slopeDirectionHint <= 4;

            if (useHint)
            {
                var proj = c.Select(pt => (pt.X * primaryDir.X) + (pt.Y * primaryDir.Y)).ToList();
                var pMin = proj.Min();
                var pMax = proj.Max();
                var pSpan = Math.Max(1e-9, pMax - pMin);
                for (var i = 0; i < 4; i++)
                {
                    var t = reverse
                        ? (pMax - proj[i]) / pSpan
                        : (proj[i] - pMin) / pSpan;
                    topByCorner[i] = topStart + ((topEnd - topStart) * t);
                }
            }
            else
            {
                var e1 = new Vector2d(c[1].X - c[0].X, c[1].Y - c[0].Y);
                var e2 = new Vector2d(c[3].X - c[0].X, c[3].Y - c[0].Y);
                var slopeDir = e1.Length >= e2.Length ? e1 : e2;
                if (slopeDir.Length <= 1e-12)
                    return false;
                slopeDir = new Vector2d(slopeDir.X / slopeDir.Length, slopeDir.Y / slopeDir.Length);

                var proj = c.Select(pt => (pt.X * slopeDir.X) + (pt.Y * slopeDir.Y)).ToList();
                var pMin = proj.Min();
                var pMax = proj.Max();
                var pSpan = Math.Max(1e-9, pMax - pMin);
                for (var i = 0; i < 4; i++)
                {
                    var t = (proj[i] - pMin) / pSpan;
                    topByCorner[i] = topStart + ((topEnd - topStart) * t);
                }
            }

            for (var i = 0; i < 4; i++)
            {
                var zTop = ToCadZ(topByCorner[i]);
                top[i] = new Point3d(c[i].X, c[i].Y, zTop);
                bot[i] = new Point3d(c[i].X, c[i].Y, zTop - thickness);
            }

            void AddFace(Point3d p1, Point3d p2, Point3d p3, Point3d p4)
            {
                var f = new Face(p1, p2, p3, p4, true, true, true, true)
                {
                    Layer = "DOSEMELER"
                };
                modelSpace.AppendEntity(f);
                tr.AddNewlyCreatedDBObject(f, true);
            }

            AddFace(top[0], top[1], top[2], top[3]); // top
            AddFace(bot[3], bot[2], bot[1], bot[0]); // bottom
            AddFace(top[0], top[3], bot[3], bot[0]); // side
            AddFace(top[1], top[0], bot[0], bot[1]); // side
            AddFace(top[2], top[1], bot[1], bot[2]); // side
            AddFace(top[3], top[2], bot[2], bot[3]); // side
            return true;
        }

        private static double DegreesToRadians(double deg)
        {
            return deg * (Math.PI / 180.0);
        }

        private static double ToCadZ(double staZ)
        {
            return staZ;
        }

        private static double NormalizeColumnHeight(
            double rawHeight,
            double storyHeightRef,
            double storyBase,
            double storyTop)
        {
            if (rawHeight <= 0.0)
                return storyHeightRef;

            // ST4 files may provide heights in m, dm, cm, or mm.
            // Some files store column second-line value as absolute top elevation (e.g. 7.00, 10.00)
            // instead of storey height. Pick best interpretation per storey.
            var candidates = new[]
            {
                rawHeight,
                rawHeight / 10.0,
                rawHeight / 100.0,
                rawHeight / 1000.0
            }
            .Where(v => v > 0.05 && v < 100.0)
            .ToList();

            if (candidates.Count == 0)
                return storyHeightRef > 0.0 ? storyHeightRef : rawHeight;

            var heightCandidate = storyHeightRef > 0.0
                ? candidates.OrderBy(v => Math.Abs(v - storyHeightRef)).First()
                : candidates.OrderBy(v => v).First();

            var hasAbsoluteContext = storyTop > (storyBase + 0.05);
            if (hasAbsoluteContext)
            {
                var absTopCandidate = candidates
                    .OrderBy(v => Math.Abs(v - storyTop))
                    .First();
                var absTopDist = Math.Abs(absTopCandidate - storyTop);
                var topMatchTolerance = Math.Max(0.25, storyHeightRef * 0.35);
                if (absTopCandidate > (storyBase + 0.05) && absTopDist <= topMatchTolerance)
                {
                    var hAbs = absTopCandidate - storyBase;
                    if (hAbs > 0.05)
                        return hAbs;
                }
            }

            return heightCandidate;
        }

        private static double ResolveColumnAxisEccOffset(double eccValue, double sectionSize)
        {
            var v = eccValue;
            if (Math.Abs(v) <= 1.5)
            {
                // ST4 code mode: -1/0/1 means shift by half section in local axis.
                return v * (sectionSize / 2.0);
            }

            // ST4 numeric mode: values are typically in mm (e.g. -400, 200, 117).
            return v / 1000.0;
        }

        private sealed class ColumnData
        {
            public ColumnData(string name, int storyNo, double storyTop, int xId, int yId, double b, double d, double h, double eccX, double eccY, double angleDeg)
            {
                Name = name;
                StoryNo = storyNo;
                StoryTop = storyTop;
                XId = xId;
                YId = yId;
                B = b;
                D = d;
                H = h;
                EccX = eccX;
                EccY = eccY;
                AngleDeg = angleDeg;
                ConnectedBeams = new List<BeamData>();
                AdjacentSlabs = new List<SlabData>();
            }

            public string Name { get; }
            public int StoryNo { get; }
            public double StoryTop { get; }
            public int XId { get; }
            public int YId { get; }
            public double B { get; }
            public double D { get; }
            public double H { get; }
            public double EccX { get; }
            public double EccY { get; }
            public double AngleDeg { get; }
            public List<BeamData> ConnectedBeams { get; }
            public List<SlabData> AdjacentSlabs { get; }

            public void Draw(
                BlockTableRecord modelSpace,
                Transaction tr,
                Dictionary<(int xId, int yId), Point2d> nodes,
                int aAxisId,
                bool hasAAxisNormal,
                Vector2d aAxisInwardNormal)
            {
                if (!nodes.TryGetValue((XId, YId), out var center))
                    return;

                var colTop = GetColumnTopFromConnectedBeams(this, StoryTop);
                var colBottom = colTop - H;
                if (colBottom < 0.0 && Math.Abs(colBottom) < 0.50)
                    colBottom = 0.0;
                var effectiveHeight = Math.Max(0.05, colTop - colBottom);

                var solid = new Solid3d();
                solid.SetDatabaseDefaults();
                solid.Layer = "KOLONLAR";
                var boxX = SwapColumnSectionAxes ? D : B;
                var boxY = SwapColumnSectionAxes ? B : D;
                var theta = DegreesToRadians(AngleDeg + ColumnRotationOffsetDeg);
                var centerShifted = center;
                if (UseColumnAxisEccentricity)
                {
                    var localOffX = ResolveColumnAxisEccOffset(EccX, boxX);
                    var localOffY = ResolveColumnAxisEccOffset(EccY, boxY);
                    var worldOffX = (localOffX * Math.Cos(theta)) - (localOffY * Math.Sin(theta));
                    var worldOffY = (localOffX * Math.Sin(theta)) + (localOffY * Math.Cos(theta));
                    centerShifted = new Point2d(center.X + worldOffX, center.Y + worldOffY);
                }

                // Keep A-axis columns entirely below/inside the A axis line.
                if (hasAAxisNormal && XId == aAxisId)
                {
                    var ux = new Vector2d(Math.Cos(theta), Math.Sin(theta));
                    var uy = new Vector2d(-Math.Sin(theta), Math.Cos(theta));
                    var halfAlongNormal =
                        0.5 * (Math.Abs(Dot(ux, aAxisInwardNormal)) * boxX +
                               Math.Abs(Dot(uy, aAxisInwardNormal)) * boxY);
                    centerShifted = new Point2d(
                        centerShifted.X + (aAxisInwardNormal.X * halfAlongNormal),
                        centerShifted.Y + (aAxisInwardNormal.Y * halfAlongNormal));
                }

                solid.CreateBox(boxX, boxY, effectiveHeight);
                solid.TransformBy(Matrix3d.Rotation(theta, Vector3d.ZAxis, Point3d.Origin));
                solid.TransformBy(Matrix3d.Displacement(new Vector3d(centerShifted.X, centerShifted.Y, 0.0)));
                AlignSolidCenter(solid, new Point3d(centerShifted.X, centerShifted.Y, ToCadZ((colBottom + colTop) / 2.0)));
                modelSpace.AppendEntity(solid);
                tr.AddNewlyCreatedDBObject(solid, true);

                var label = new DBText
                {
                    Layer = "ETIKETLER",
                    Height = 0.20,
                    Position = new Point3d(centerShifted.X, centerShifted.Y, ToCadZ(colTop + 0.2)),
                    TextString = Name
                };
                modelSpace.AppendEntity(label);
                tr.AddNewlyCreatedDBObject(label, true);
            }
        }

        private sealed class BeamData
        {
            public BeamData(string name, int storyNo, double storyTop, int axis, int start, int end, double b, double h, int ecc, double dz)
            {
                Name = name;
                StoryNo = storyNo;
                StoryTop = storyTop;
                Axis = axis;
                Start = start;
                End = end;
                B = b;
                H = h;
                Ecc = ecc;
                Dz = dz;
                ConnectedColumns = new List<ColumnData>();
                AdjacentSlabs = new List<SlabData>();
                NeighborBeams = new List<BeamData>();
            }

            public string Name { get; }
            public int StoryNo { get; }
            public double StoryTop { get; }
            public int Axis { get; }
            public int Start { get; }
            public int End { get; }
            public double B { get; }
            public double H { get; }
            public int Ecc { get; }
            public double Dz { get; }
            public List<ColumnData> ConnectedColumns { get; }
            public List<SlabData> AdjacentSlabs { get; }
            public List<BeamData> NeighborBeams { get; }

            public void Draw(
                BlockTableRecord modelSpace,
                Transaction tr,
                Dictionary<(int xId, int yId), Point2d> nodes,
                Dictionary<int, double> xBase)
            {
                Point2d p1;
                Point2d p2;

                if (xBase.ContainsKey(Axis))
                {
                    if (!nodes.TryGetValue((Axis, Start), out p1) || !nodes.TryGetValue((Axis, End), out p2))
                        return;
                }
                else
                {
                    if (!nodes.TryGetValue((Start, Axis), out p1) || !nodes.TryGetValue((End, Axis), out p2))
                        return;
                }

                var dx = p2.X - p1.X;
                var dy = p2.Y - p1.Y;
                var len = Math.Sqrt((dx * dx) + (dy * dy));
                if (len <= 1e-12)
                    return;

                var (ox, oy) = ComputeBeamEccOffset(dx, dy, B, Ecc);
                var px1 = p1.X + ox;
                var py1 = p1.Y + oy;
                var px2 = p2.X + ox;
                var py2 = p2.Y + oy;
                var angle = Math.Atan2(py2 - py1, px2 - px1);

                var zTopBeam = StoryTop + Dz;
                var zBottomBeam = zTopBeam - H;
                var cadZTopBeam = ToCadZ(zTopBeam);
                var cadZBottomBeam = ToCadZ(zBottomBeam);

                var solid = new Solid3d();
                solid.SetDatabaseDefaults();
                solid.Layer = "KIRISLER";
                solid.CreateBox(len, B, H);
                solid.TransformBy(Matrix3d.Rotation(angle, Vector3d.ZAxis, Point3d.Origin));
                solid.TransformBy(Matrix3d.Displacement(new Vector3d(px1, py1, cadZBottomBeam)));
                AlignSolidCenter(solid, new Point3d(
                    (px1 + px2) / 2.0,
                    (py1 + py2) / 2.0,
                    (cadZBottomBeam + cadZTopBeam) / 2.0));
                modelSpace.AppendEntity(solid);
                tr.AddNewlyCreatedDBObject(solid, true);

                var label = new DBText
                {
                    Layer = "ETIKETLER",
                    Height = 0.20,
                    Position = new Point3d((px1 + px2) / 2.0, (py1 + py2) / 2.0, ToCadZ(zTopBeam + 0.1)),
                    TextString = Name
                };
                modelSpace.AppendEntity(label);
                tr.AddNewlyCreatedDBObject(label, true);
            }
        }

        private sealed class SlabData
        {
            public SlabData(
                string name,
                int storyNo,
                double storyTop,
                int slabType,
                int stairTag,
                int stairFlag,
                int x1,
                int x2,
                int y1,
                int y2,
                double thickness,
                double dropStart,
                double dropEnd)
            {
                Name = name;
                StoryNo = storyNo;
                StoryTop = storyTop;
                SlabType = slabType;
                StairTag = stairTag;
                StairFlag = stairFlag;
                X1 = x1;
                X2 = x2;
                Y1 = y1;
                Y2 = y2;
                Thickness = thickness;
                DropStart = dropStart;
                DropEnd = dropEnd;
                BoundaryBeams = new List<BeamData>();
                NeighborBeams = new List<BeamData>();
                NeighborSlabs = new List<SlabData>();
            }

            public string Name { get; }
            public int StoryNo { get; }
            public double StoryTop { get; }
            public int SlabType { get; }
            public int StairTag { get; }
            public int StairFlag { get; }
            public int X1 { get; }
            public int X2 { get; }
            public int Y1 { get; }
            public int Y2 { get; }
            public double Thickness { get; }
            public double DropStart { get; }
            public double DropEnd { get; }
            public bool IsStairSlab => SlabType == 2 || StairTag > 0 || StairFlag > 0;
            public List<BeamData> BoundaryBeams { get; }
            public List<BeamData> NeighborBeams { get; }
            public List<SlabData> NeighborSlabs { get; }

            public void Draw(
                BlockTableRecord modelSpace,
                Transaction tr,
                Dictionary<(int xId, int yId), Point2d> nodes,
                Dictionary<int, double> xBase,
                List<BeamData> beams)
            {
                if (!nodes.TryGetValue((X1, Y1), out var p1) ||
                    !nodes.TryGetValue((X2, Y1), out var p2) ||
                    !nodes.TryGetValue((X2, Y2), out var p3) ||
                    !nodes.TryGetValue((X1, Y2), out var p4))
                    return;
                if (Thickness <= 1e-9)
                    return;

                var storyBeams = beams
                    .Where(b => b.StoryNo == StoryNo)
                    .ToList();
                var cx = (p1.X + p2.X + p3.X + p4.X) / 4.0;
                var cy = (p1.Y + p2.Y + p3.Y + p4.Y) / 4.0;
                List<Point2d> inwardCorners;
                if (!TryBuildSlabCornersFromAxisBoundaries(this, nodes, xBase, storyBeams, out inwardCorners) &&
                    !TryBuildSlabCornersFromBeams(p1, p2, p3, p4, nodes, xBase, storyBeams, out inwardCorners))
                {
                    var verticalAxes = new HashSet<int>(xBase.Keys);
                    var leftInset = GetBoundaryBeamHalfWidth(storyBeams, verticalAxes, true, X1, Y1, Y2);
                    var rightInset = GetBoundaryBeamHalfWidth(storyBeams, verticalAxes, true, X2, Y1, Y2);
                    var bottomInset = GetBoundaryBeamHalfWidth(storyBeams, verticalAxes, false, Y1, X1, X2);
                    var topInset = GetBoundaryBeamHalfWidth(storyBeams, verticalAxes, false, Y2, X1, X2);

                    inwardCorners = BuildInsetSlabCorners(
                        p1, p2, p3, p4,
                        leftInset, rightInset, bottomInset, topInset);
                }

                inwardCorners = OrderCornersClockwise(inwardCorners);
                var fallbackCorners = OrderCornersClockwise(new[] { p1, p2, p3, p4 });
                if (PolygonAreaAbs(inwardCorners) <= 1e-9)
                    inwardCorners = fallbackCorners;

                var hasSlope = Math.Abs(DropEnd - DropStart) > 1e-9;
                var zTopSlab = StoryTop + DropStart;
                var zBottomSlab = zTopSlab - Thickness;
                var cadZTopSlab = ToCadZ(zTopSlab);
                var cadZBottomSlab = ToCadZ(zBottomSlab);
                bool TryCreateSlabSolid(IReadOnlyList<Point2d> corners)
                {
                    if (corners == null || corners.Count < 4 || PolygonAreaAbs(corners) <= 1e-9)
                        return false;

                    // AlignSolidCenter uses extents center; use the same reference in XY to avoid drift on skewed slabs.
                    var slabCenterX = (corners.Min(pt => pt.X) + corners.Max(pt => pt.X)) / 2.0;
                    var slabCenterY = (corners.Min(pt => pt.Y) + corners.Max(pt => pt.Y)) / 2.0;

                    using (var pl = new Polyline(4))
                    {
                        pl.AddVertexAt(0, corners[0], 0.0, 0.0, 0.0);
                        pl.AddVertexAt(1, corners[1], 0.0, 0.0, 0.0);
                        pl.AddVertexAt(2, corners[2], 0.0, 0.0, 0.0);
                        pl.AddVertexAt(3, corners[3], 0.0, 0.0, 0.0);
                        pl.Closed = true;
                        pl.Elevation = cadZBottomSlab;

                        var curveSegments = new DBObjectCollection();
                        curveSegments.Add(pl);
                        var regions = Region.CreateFromCurves(curveSegments);
                        if (regions.Count == 0)
                            return false;

                        using (var region = regions[0] as Region)
                        {
                            if (region == null)
                                return false;

                            var solid = new Solid3d();
                            solid.SetDatabaseDefaults();
                            solid.Layer = "DOSEMELER";
                            solid.Extrude(region, Thickness, 0.0);
                            AlignSolidCenter(solid, new Point3d(
                                slabCenterX,
                                slabCenterY,
                                (cadZBottomSlab + cadZTopSlab) / 2.0));
                            modelSpace.AppendEntity(solid);
                            tr.AddNewlyCreatedDBObject(solid, true);
                            return true;
                        }
                    }
                }

                // Draw as sloped only when slab is explicitly stair-like in stable fields.
                // StairTag alone may be used for other metadata in some ST4 variants.
                var isExplicitStairSlab = (SlabType == 2) || (StairFlag > 0);
                if (isExplicitStairSlab && hasSlope)
                {
                    // For broken/sloped slabs, inset intersection corners may become twisted
                    // on skewed grids and create folded/star-like faces.
                    // Prefer inset corners so slab sits inside boundary beams;
                    // fallback to original panel corners if inset polygon is invalid.
                    var slopeCorners = inwardCorners;
                    if (slopeCorners == null || slopeCorners.Count < 4 || PolygonAreaAbs(slopeCorners) <= 1e-9)
                        slopeCorners = fallbackCorners;
                    // ST4 stair slope direction is opposite of current CAD orientation in this project.
                    // Swap start/end levels so broken slabs are drawn in the intended direction.
                    if (!TryCreateSlopedSlabFaces(modelSpace, tr, slopeCorners, Thickness, StoryTop + DropEnd, StoryTop + DropStart, StairTag))
                    {
                        // Fallback to flat slab if sloped meshing fails; do not drop slab.
                        if (!TryCreateSlabSolid(inwardCorners) && !TryCreateSlabSolid(fallbackCorners))
                            throw new InvalidOperationException("Egimli ve duz doseme geometrisi olusturulamadi.");
                    }
                }
                else
                {
                    if (!TryCreateSlabSolid(inwardCorners) && !TryCreateSlabSolid(fallbackCorners))
                        throw new InvalidOperationException("Doseme geometrisi olusturulamadi.");
                }

                var label = new DBText
                {
                    Layer = "ETIKETLER",
                    Height = 0.20,
                    Position = new Point3d(cx, cy, ToCadZ((StoryTop + ((DropStart + DropEnd) * 0.5)) + 0.1)),
                    TextString = Name
                };
                modelSpace.AppendEntity(label);
                tr.AddNewlyCreatedDBObject(label, true);
            }
        }
    }
}
