#!/usr/bin/env python3
"""Find parenthesis imbalance in LISP file. Ignores parens in comments and strings."""

def analyze_parens(filepath):
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        content = f.read()
    
    depth = 0
    line = 1
    col = 0
    in_comment = False
    in_string = False
    escape_next = False
    string_char = None
    last_open_line = None
    last_open_col = None
    max_depth = 0
    max_depth_line = None
    max_depth_col = None
    
    i = 0
    while i < len(content):
        c = content[i]
        prev_col = col
        
        if c == '\n':
            line += 1
            col = 0
            in_comment = False
            i += 1
            continue
        
        col += 1
        
        if escape_next:
            escape_next = False
            i += 1
            continue
            
        if in_comment:
            i += 1
            continue
            
        if in_string:
            if c == '\\':
                escape_next = True
            elif c == string_char:
                in_string = False
                string_char = None
            i += 1
            continue
            
        if c == ';':
            in_comment = True
            i += 1
            continue
            
        if c == '"':
            in_string = True
            string_char = c
            i += 1
            continue
            
        if c == '(':
            depth += 1
            last_open_line = line
            last_open_col = col
            if depth > max_depth:
                max_depth = depth
                max_depth_line = line
                max_depth_col = col
        elif c == ')':
            depth -= 1
            if depth < 0:
                return {
                    'error': 'extra_close',
                    'line': line,
                    'col': col,
                    'message': f'Extra ) at line {line}, column {col} - depth went negative'
                }
        
        i += 1
    
    if depth > 0:
        return {
            'error': 'extra_open',
            'line': last_open_line,
            'col': last_open_col,
            'depth_at_eof': depth,
            'message': f'Extra ( - {depth} unclosed. Last ( opened at line {last_open_line}, column {last_open_col}. Missing ) should close it.'
        }
    
    return {'error': None, 'message': 'Balanced'}
    
if __name__ == '__main__':
    import sys
    path = sys.argv[1] if len(sys.argv) > 1 else r'c:\Users\BEYKENT\Documents\CURSOR\DENEME_OSM\ST4_Aks_Ciz.lsp'
    result = analyze_parens(path)
    print(result['message'])
    if result.get('depth_at_eof'):
        print(f"Depth at EOF: {result['depth_at_eof']}")
