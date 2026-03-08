#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ST4 dosyasından aks verilerini analiz eder ve aksları çizer.
STA4CAD ST4 formatı: /Axis data/ bölümünde her satır "0, <koordinat>, 0, 0, 0"
İlk 27 aks = 1001 serisi (X yönü), kalanlar = 2001 serisi (Y yönü)
"""

import re
import sys
from pathlib import Path


def parse_st4_axes(st4_path: str) -> tuple[list[float], list[float]]:
    """
    ST4 dosyasından aks koordinatlarını okur.
    Returns: (axis_1001_list, axis_2001_list)
    - axis_1001: X yönündeki akslar (dikey çizgiler)
    - axis_2001: Y yönündeki akslar (yatay çizgiler)
    """
    axis_positions = []
    in_axis_section = False

    with open(st4_path, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            line = line.strip()
            if "/Axis data/" in line or "/Axis Data/" in line:
                in_axis_section = True
                continue
            if in_axis_section and ("/Circle Axis/" in line or line.startswith("/")):
                break
            if in_axis_section and line:
                parts = [p.strip() for p in line.split(",")]
                if len(parts) >= 2:
                    try:
                        coord = float(parts[1])
                        axis_positions.append(coord)
                    except ValueError:
                        pass

    n = len(axis_positions)
    if n >= 27:
        n1, n2 = 27, min(9, n - 27)
    else:
        n1 = max(1, n // 2)
        n2 = n - n1
        if n2 < 1:
            n2, n1 = 1, n - 1

    axis_1001 = axis_positions[:n1]
    axis_2001 = axis_positions[n1 : n1 + n2] if n2 > 0 else []

    return axis_1001, axis_2001


def draw_axes_svg(axis_1001: list[float], axis_2001: list[float], output_path: str):
    """Aksları SVG olarak çizer."""
    if not axis_1001 or not axis_2001:
        print("Eksik aks verisi - çizim yapılamıyor.")
        return

    # Ölçek: 1m = 20px, padding
    scale = 20
    pad = 40

    x_vals = sorted(axis_1001)
    y_vals = sorted(axis_2001)
    x_min, x_max = min(x_vals), max(x_vals)
    y_min, y_max = min(y_vals), max(y_vals)

    def to_svg_x(x):
        return pad + (x - x_min) * scale

    def to_svg_y(y):
        return pad + (y_max - y) * scale  # Y aşağı

    w = int((x_max - x_min) * scale) + 2 * pad
    h = int((y_max - y_min) * scale) + 2 * pad

    lines = []
    lines.append('<?xml version="1.0" encoding="UTF-8"?>')
    lines.append(f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}">')
    lines.append('  <defs>')
    lines.append('    <style>')
    lines.append('      .axis-1001 { stroke: #2563eb; stroke-width: 1.5; fill: none; }')
    lines.append('      .axis-2001 { stroke: #dc2626; stroke-width: 1.5; fill: none; }')
    lines.append('      .label { font: 10px sans-serif; fill: #374151; }')
    lines.append('    </style>')
    lines.append('  </defs>')
    lines.append('  <rect width="100%" height="100%" fill="#fafafa"/>')

    # X aksları (1001): dikey çizgiler
    for i, x in enumerate(axis_1001):
        sx = to_svg_x(x)
        y1, y2 = to_svg_y(y_min), to_svg_y(y_max)
        lines.append(f'  <line class="axis-1001" x1="{sx}" y1="{y1}" x2="{sx}" y2="{y2}"/>')
        lines.append(f'  <text class="label" x="{sx}" y="{to_svg_y(y_min)-5}" text-anchor="middle">{i+1}</text>')

    # Y aksları (2001): yatay çizgiler
    for i, y in enumerate(axis_2001):
        sy = to_svg_y(y)
        x1, x2 = to_svg_x(x_min), to_svg_x(x_max)
        lines.append(f'  <line class="axis-2001" x1="{x1}" y1="{sy}" x2="{x2}" y2="{sy}"/>')
        lines.append(f'  <text class="label" x="{to_svg_x(x_min)-5}" y="{sy+4}" text-anchor="end">{i+1}</text>')

    lines.append("</svg>")

    Path(output_path).write_text("\n".join(lines), encoding="utf-8")
    print(f"SVG kaydedildi: {output_path}")


def main():
    st4_file = Path(__file__).parent / "CC_02.ST4"
    if len(sys.argv) > 1:
        st4_file = Path(sys.argv[1])

    if not st4_file.exists():
        print(f"Dosya bulunamadı: {st4_file}")
        return 1

    axis_1001, axis_2001 = parse_st4_axes(str(st4_file))

    print("=== ST4 AKS ANALİZİ ===")
    print(f"Aks 1001 (X yönü) sayısı: {len(axis_1001)}")
    print(f"  Koordinatlar: {[round(x, 2) for x in axis_1001]}")
    print(f"Aks 2001 (Y yönü) sayısı: {len(axis_2001)}")
    print(f"  Koordinatlar: {[round(y, 2) for y in axis_2001]}")

    out_svg = st4_file.with_suffix(".aks.svg")
    draw_axes_svg(axis_1001, axis_2001, str(out_svg))

    return 0


if __name__ == "__main__":
    sys.exit(main())
