import React from 'react';

// eslint-disable-next-line react/prop-types
export default function Layout({children}) {
  return (
    <div style={{
      margin: `0 auto`, maxWidth: 650, padding: `0 1rem`
    }}>
      {children}
    </div>
  )
}
