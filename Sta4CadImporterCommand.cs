using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Rhino;
using Rhino.Commands;
using Rhino.DocObjects;
using Rhino.Geometry;
using Rhino.UI;

namespace BetonLispRhino
{
    public class Sta4CadImporterCommand : Command
    {
        public override string EnglishName => "ImportSTA4CAD";

        protected override Result RunCommand(RhinoDoc doc, RunMode mode)
        {
            var fileDialog = new OpenFileDialog
            {
                Title = "STA4CAD Projesi Sec",
                Filter = "STA4CAD Files (*.st4)|*.st4"
            };

            if (!fileDialog.ShowOpenDialog())
                return Result.Cancel;

            var filePath = fileDialog.FileName;
            if (string.IsNullOrWhiteSpace(filePath) || !File.Exists(filePath))
            {
                RhinoApp.WriteLine("Gecerli bir .st4 dosyasi secilemedi.");
                return Result.Failure;
            }

            var lines = File.ReadAllLines(filePath);
            var content = string.Join(Environment.NewLine, lines);

            var sections = ParseSections(lines);
            var axisData = GetSection(sections, "/Axis data");
            var colAxisData = GetSection(sections, "/Column axis");
            var colLines = GetSection(sections, "/Columns Data");
            var beamLines = GetSection(sections, "/Beams Data");
            var slabLines = GetSection(sections, "/Floors Data");
            var storyData = GetSection(sections, "/Story");

            if (axisData.Count == 0)
            {
                RhinoApp.WriteLine("Dosyada Aks verisi bulunamadi.");
                return Result.Failure;
            }

            var zTop = 3.0;
            if (storyData.Count > 1)
            {
                var parts = storyData[1].Split(',');
                if (parts.Length >= 3)
                    zTop = ParseDouble(parts[2], 3.0);
            }

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
                yCoords[1001 + i] = ParseDouble(p[1], 0.0);
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
                    var x = Math.Abs(skew) > RhinoMath.ZeroTolerance ? baseX + (skew * y) : baseX;
                    nodes[(xId, yId)] = new Point2d(x, y);
                }
            }

            var colMapping = new Dictionary<int, (int yId, int xId, double rot)>();
            for (var i = 0; i < colAxisData.Count; i++)
            {
                var p = colAxisData[i].Split(',');
                if (p.Length > 6)
                {
                    colMapping[i + 1] = (
                        ParseInt(p[1], 0),
                        ParseInt(p[2], 0),
                        ParseDouble(p[6], 0.0)
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
                var name = "S" + GetPart(p1, 0);
                var b = ParseDouble(GetPart(p1, 1), 0.0) / 100.0;
                var d = ParseDouble(GetPart(p1, 2), 0.0) / 100.0;
                var h = zTop;

                if (el.Count > 1 && el[1].Length > 6)
                {
                    var hVal = ParseDouble(GetPart(el[1], 6), -1.0);
                    if (hVal > 0.0)
                        h = hVal;
                }

                if (!colMapping.TryGetValue(i + 1, out var map))
                    continue;

                columns.Add(new ColumnData(name, map.xId, map.yId, b, d, h, map.rot));
            }

            var beams = new List<BeamData>();
            var beamBlocks = GroupElements(beamLines);
            foreach (var el in beamBlocks)
            {
                if (el.Count == 0)
                    continue;

                var p1 = el[0];
                var name = "K" + GetPart(p1, 0);
                var b = ParseDouble(GetPart(p1, 1), 0.0) / 100.0;
                var h = ParseDouble(GetPart(p1, 2), 0.0) / 100.0;
                var axis = ParseInt(GetPart(p1, 4), 0);
                var start = ParseInt(GetPart(p1, 5), 0);
                var end = ParseInt(GetPart(p1, 6), 0);
                var ecc = ParseInt(GetPart(p1, 7), 0);
                var dz = ParseDouble(GetPart(p1, 11), 0.0) / 100.0;

                var ox = 0.0;
                var oy = 0.0;
                var isVertical = xBase.ContainsKey(axis);
                if (ecc == -1)
                {
                    if (isVertical) ox = b / 2.0;
                    else oy = b / 2.0;
                }
                else if (ecc == 1)
                {
                    if (isVertical) ox = -b / 2.0;
                    else oy = -b / 2.0;
                }

                beams.Add(new BeamData(name, axis, start, end, b, h, ox, oy, dz));
            }

            var slabs = new List<SlabData>();
            var slabBlocks = GroupElements(slabLines);
            foreach (var el in slabBlocks)
            {
                if (el.Count == 0)
                    continue;

                var p1 = el[0];
                var name = "D" + GetPart(p1, 0);
                var thick = ParseDouble(GetPart(p1, 1), 0.0) / 100.0;
                var drop = ParseDouble(GetPart(p1, 5), 0.0) / 100.0;

                slabs.Add(new SlabData(
                    name,
                    ParseInt(GetPart(p1, 10), 0),
                    ParseInt(GetPart(p1, 11), 0),
                    ParseInt(GetPart(p1, 8), 0),
                    ParseInt(GetPart(p1, 9), 0),
                    thick,
                    drop
                ));
            }

            var layers = new[] { "KOLONLAR", "KIRISLER", "DOSEMELER", "AKSLAR", "ETIKETLER" };
            var colors = new[]
            {
                System.Drawing.Color.FromArgb(255, 0, 0),
                System.Drawing.Color.FromArgb(0, 0, 255),
                System.Drawing.Color.FromArgb(100, 200, 100),
                System.Drawing.Color.FromArgb(255, 200, 0),
                System.Drawing.Color.FromArgb(0, 255, 255)
            };

            for (var i = 0; i < layers.Length; i++)
            {
                var idx = doc.Layers.FindByFullPath(layers[i], -1);
                if (idx < 0)
                {
                    var layer = new Layer { Name = layers[i], Color = colors[i] };
                    doc.Layers.Add(layer);
                }
            }

            foreach (var layerName in layers)
            {
                DeleteObjectsOnLayer(doc, layerName);
            }

            var ext = 2.0;
            DrawAxes(doc, nodes, yCoords, xBase, ext);

            foreach (var c in columns)
            {
                if (!nodes.TryGetValue((c.XId, c.YId), out var center))
                    continue;

                var corners = CreateOrientedPrismCorners(center.X, center.Y, 0.0, c.H, c.B, c.D, c.AngleDeg);
                var brep = Brep.CreateFromBox(corners);
                if (brep == null)
                    continue;

                SetCurrentLayer(doc, "KOLONLAR");
                doc.Objects.AddBrep(brep);
                SetCurrentLayer(doc, "ETIKETLER");
                doc.Objects.AddTextDot(c.Name, new Point3d(center.X, center.Y, c.H + 0.2));
            }

            foreach (var b in beams)
            {
                Point2d p1;
                Point2d p2;

                if (xBase.ContainsKey(b.Axis))
                {
                    if (!nodes.TryGetValue((b.Axis, b.Start), out p1) || !nodes.TryGetValue((b.Axis, b.End), out p2))
                        continue;
                }
                else
                {
                    if (!nodes.TryGetValue((b.Start, b.Axis), out p1) || !nodes.TryGetValue((b.End, b.Axis), out p2))
                        continue;
                }

                var dx = p2.X - p1.X;
                var dy = p2.Y - p1.Y;
                var length = Math.Sqrt((dx * dx) + (dy * dy));
                if (length <= RhinoMath.ZeroTolerance)
                    continue;

                var ux = dx / length;
                var uy = dy / length;
                var vx = -uy;
                var vy = ux;

                var zTopBeam = zTop + b.Dz;
                var zBottomBeam = zTopBeam - b.H;
                var px1 = p1.X + b.Ox;
                var py1 = p1.Y + b.Oy;
                var px2 = p2.X + b.Ox;
                var py2 = p2.Y + b.Oy;

                var pts = new List<Point3d>
                {
                    new Point3d(px1 + vx * b.B / 2.0, py1 + vy * b.B / 2.0, zBottomBeam),
                    new Point3d(px2 + vx * b.B / 2.0, py2 + vy * b.B / 2.0, zBottomBeam),
                    new Point3d(px2 - vx * b.B / 2.0, py2 - vy * b.B / 2.0, zBottomBeam),
                    new Point3d(px1 - vx * b.B / 2.0, py1 - vy * b.B / 2.0, zBottomBeam),
                    new Point3d(px1 + vx * b.B / 2.0, py1 + vy * b.B / 2.0, zTopBeam),
                    new Point3d(px2 + vx * b.B / 2.0, py2 + vy * b.B / 2.0, zTopBeam),
                    new Point3d(px2 - vx * b.B / 2.0, py2 - vy * b.B / 2.0, zTopBeam),
                    new Point3d(px1 - vx * b.B / 2.0, py1 - vy * b.B / 2.0, zTopBeam)
                };

                var brep = Brep.CreateFromBox(pts);
                if (brep == null)
                    continue;

                SetCurrentLayer(doc, "KIRISLER");
                doc.Objects.AddBrep(brep);
                SetCurrentLayer(doc, "ETIKETLER");
                doc.Objects.AddTextDot(b.Name, new Point3d((px1 + px2) / 2.0, (py1 + py2) / 2.0, zTopBeam + 0.1));
            }

            foreach (var s in slabs)
            {
                if (!nodes.TryGetValue((s.X1, s.Y1), out var p1) ||
                    !nodes.TryGetValue((s.X2, s.Y1), out var p2) ||
                    !nodes.TryGetValue((s.X2, s.Y2), out var p3) ||
                    !nodes.TryGetValue((s.X1, s.Y2), out var p4))
                    continue;

                var cx = (p1.X + p2.X + p3.X + p4.X) / 4.0;
                var cy = (p1.Y + p2.Y + p3.Y + p4.Y) / 4.0;

                var op1 = OffsetTowardCentroid(p1, cx, cy);
                var op2 = OffsetTowardCentroid(p2, cx, cy);
                var op3 = OffsetTowardCentroid(p3, cx, cy);
                var op4 = OffsetTowardCentroid(p4, cx, cy);

                var zTopSlab = zTop + s.Drop;
                var zBottomSlab = zTopSlab - s.Thickness;

                var slabCorners = new List<Point3d>
                {
                    new Point3d(op1.X, op1.Y, zBottomSlab),
                    new Point3d(op2.X, op2.Y, zBottomSlab),
                    new Point3d(op3.X, op3.Y, zBottomSlab),
                    new Point3d(op4.X, op4.Y, zBottomSlab),
                    new Point3d(op1.X, op1.Y, zTopSlab),
                    new Point3d(op2.X, op2.Y, zTopSlab),
                    new Point3d(op3.X, op3.Y, zTopSlab),
                    new Point3d(op4.X, op4.Y, zTopSlab)
                };

                var brep = Brep.CreateFromBox(slabCorners);
                if (brep == null)
                    continue;

                SetCurrentLayer(doc, "DOSEMELER");
                doc.Objects.AddBrep(brep);
                SetCurrentLayer(doc, "ETIKETLER");
                doc.Objects.AddTextDot(s.Name, new Point3d(cx, cy, zTopSlab + 0.1));
            }

            SetCurrentLayer(doc, "Default");
            doc.Views.Redraw();
            RhinoApp.RunScript("_Zoom _Extents", false);

            RhinoApp.WriteLine("Dinamik Tarama Basarili!");
            RhinoApp.WriteLine($"Cizilen Kolon: {columns.Count}");
            RhinoApp.WriteLine($"Cizilen Kiris: {beams.Count}");
            RhinoApp.WriteLine($"Cizilen Doseme: {slabs.Count}");

            return Result.Success;
        }

        private static Dictionary<string, List<string>> ParseSections(IEnumerable<string> lines)
        {
            var sections = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
            var current = string.Empty;

            foreach (var rawLine in lines)
            {
                var line = rawLine?.Trim() ?? string.Empty;
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

        private static void DeleteObjectsOnLayer(RhinoDoc doc, string layerName)
        {
            var layerIndex = doc.Layers.FindByFullPath(layerName, -1);
            if (layerIndex < 0)
                return;

            var settings = new ObjectEnumeratorSettings
            {
                LayerIndexFilter = layerIndex,
                ActiveObjects = true,
                HiddenObjects = true,
                LockedObjects = true
            };

            var ids = doc.Objects.GetObjectList(settings).Select(o => o.Id).ToList();
            if (ids.Count == 0)
                return;

            foreach (var id in ids)
                doc.Objects.Delete(id, true);
        }

        private static void SetCurrentLayer(RhinoDoc doc, string layerName)
        {
            var idx = doc.Layers.FindByFullPath(layerName, -1);
            if (idx >= 0)
                doc.Layers.SetCurrentLayerIndex(idx, true);
        }

        private static void DrawAxes(
            RhinoDoc doc,
            Dictionary<(int xId, int yId), Point2d> nodes,
            Dictionary<int, double> yCoords,
            Dictionary<int, double> xBase,
            double ext)
        {
            if (yCoords.Count == 0 || xBase.Count == 0 || nodes.Count == 0)
                return;

            SetCurrentLayer(doc, "AKSLAR");

            var minY = yCoords.Values.Min();
            var maxY = yCoords.Values.Max();
            var allX = new List<double>();
            foreach (var xid in xBase.Keys)
            {
                foreach (var yid in yCoords.Keys)
                {
                    if (nodes.TryGetValue((xid, yid), out var p))
                        allX.Add(p.X);
                }
            }

            if (allX.Count == 0)
                return;

            var minX = allX.Min();
            var maxX = allX.Max();

            foreach (var yEntry in yCoords)
            {
                var yId = yEntry.Key;
                var y = yEntry.Value;

                var start = new Point3d(minX - ext, y, 0.0);
                var end = new Point3d(maxX + ext, y, 0.0);
                doc.Objects.AddLine(start, end);
                doc.Objects.AddTextDot(yId.ToString(CultureInfo.InvariantCulture), start);
                doc.Objects.AddTextDot(yId.ToString(CultureInfo.InvariantCulture), end);
            }

            var minYId = yCoords.Keys.Min();
            var maxYId = yCoords.Keys.Max();
            foreach (var xId in xBase.Keys)
            {
                if (!nodes.TryGetValue((xId, minYId), out var p1) || !nodes.TryGetValue((xId, maxYId), out var p2))
                    continue;

                var dx = p2.X - p1.X;
                var dy = p2.Y - p1.Y;
                var length = Math.Sqrt((dx * dx) + (dy * dy));
                if (length <= RhinoMath.ZeroTolerance)
                    continue;

                var ux = dx / length;
                var uy = dy / length;
                var start = new Point3d(p1.X - (ux * ext), p1.Y - (uy * ext), 0.0);
                var end = new Point3d(p2.X + (ux * ext), p2.Y + (uy * ext), 0.0);

                doc.Objects.AddLine(start, end);
                doc.Objects.AddTextDot(xId.ToString(CultureInfo.InvariantCulture), start);
                doc.Objects.AddTextDot(xId.ToString(CultureInfo.InvariantCulture), end);
            }
        }

        private static List<Point3d> CreateOrientedPrismCorners(
            double cx,
            double cy,
            double z0,
            double z1,
            double b,
            double d,
            double angleDeg)
        {
            var rad = RhinoMath.ToRadians(angleDeg);
            var cosA = Math.Cos(rad);
            var sinA = Math.Sin(rad);
            var hb = b / 2.0;
            var hd = d / 2.0;

            var local = new[]
            {
                new Point2d(-hb, -hd),
                new Point2d(hb, -hd),
                new Point2d(hb, hd),
                new Point2d(-hb, hd)
            };

            var pts = new List<Point3d>(8);
            foreach (var z in new[] { z0, z1 })
            {
                foreach (var p in local)
                {
                    var x = cx + (p.X * cosA) - (p.Y * sinA);
                    var y = cy + (p.X * sinA) + (p.Y * cosA);
                    pts.Add(new Point3d(x, y, z));
                }
            }

            return pts;
        }

        private static Point2d OffsetTowardCentroid(Point2d p, double cx, double cy, double dist = 0.20)
        {
            var dx = cx - p.X;
            var dy = cy - p.Y;
            var length = Math.Sqrt((dx * dx) + (dy * dy));
            if (length <= RhinoMath.ZeroTolerance)
                return p;

            var move = Math.Min(dist, length * 0.4);
            return new Point2d(p.X + (dx / length) * move, p.Y + (dy / length) * move);
        }

        private readonly struct ColumnData
        {
            public ColumnData(string name, int xId, int yId, double b, double d, double h, double angleDeg)
            {
                Name = name;
                XId = xId;
                YId = yId;
                B = b;
                D = d;
                H = h;
                AngleDeg = angleDeg;
            }

            public string Name { get; }
            public int XId { get; }
            public int YId { get; }
            public double B { get; }
            public double D { get; }
            public double H { get; }
            public double AngleDeg { get; }
        }

        private readonly struct BeamData
        {
            public BeamData(string name, int axis, int start, int end, double b, double h, double ox, double oy, double dz)
            {
                Name = name;
                Axis = axis;
                Start = start;
                End = end;
                B = b;
                H = h;
                Ox = ox;
                Oy = oy;
                Dz = dz;
            }

            public string Name { get; }
            public int Axis { get; }
            public int Start { get; }
            public int End { get; }
            public double B { get; }
            public double H { get; }
            public double Ox { get; }
            public double Oy { get; }
            public double Dz { get; }
        }

        private readonly struct SlabData
        {
            public SlabData(string name, int x1, int x2, int y1, int y2, double thickness, double drop)
            {
                Name = name;
                X1 = x1;
                X2 = x2;
                Y1 = y1;
                Y2 = y2;
                Thickness = thickness;
                Drop = drop;
            }

            public string Name { get; }
            public int X1 { get; }
            public int X2 { get; }
            public int Y1 { get; }
            public int Y2 { get; }
            public double Thickness { get; }
            public double Drop { get; }
        }
    }
}
